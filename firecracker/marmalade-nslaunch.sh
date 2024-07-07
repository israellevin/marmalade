#!/bin/bash
NAMESPACE_PREFIX=marmalade
VETH_PREFIX=gen.veth.

usage() {
    echo -en "Usage:\n" \
        "$0 --start-jam <jam name>\n" \
        "$0 --launch-generator <jam name> <player ID> <generator name> <instance ID>\n" \
        "$0 --stop-jam <jam name>\n" \
        "$0 --help\n"
    exit $1
}

start_jam() {
    local jam_namespace="$NAMESPACE_PREFIX:$1"
    # Exit if the namespace already exists.
    ip netns list | grep -q "^$jam_namespace *" && exit 1
    # Create the namespace.
    ip netns add "$jam_namespace"
    # Set up the loopback interface.
    ip netns exec "$jam_namespace" ip link set lo up
    # Launch the Redis server.
    ip netns exec "$jam_namespace" \
        redis-server --dir "./jams/$1" --save '' --appendonly yes --appenddirname redis --daemonize yes --protected-mode no
}

increment_ip() {
    local -a octets
    IFS='.' read -r -a octets <<< "$1"
    for (( i=3; i>=0; i-- )); do
        [ $((++octets[i])) -lt 256 ] && break
        [ $i -eq 0 ] && echo "Error: IP address overflow" && exit 1
        octets[i]=0
    done
    echo "${octets[0]}.${octets[1]}.${octets[2]}.${octets[3]}"
}

next_veth() {
    local jam_namespace="$NAMESPACE_PREFIX:$1"
    # Exit if the jam namespace does not exist.
    ip netns list | grep -q "^$jam_namespace *" || exit 1
    # Get the highest numbered prefixed interface in the namespace.
    local max_veth=$(ip netns exec "$jam_namespace" ip -o link show | cut -d' ' -f2 | \
        grep -o "^$VETH_PREFIX[0-9]\+@" | sort -n | tail -1 | head -c-2)
    if ! [ "$max_veth" ]; then
        echo "$VETH_PREFIX"'0 10.24.68.0 10.24.68.1'
        return
    fi
    local max_veth_number=${max_veth:${#VETH_PREFIX}}
    local new_generator_ifname="$VETH_PREFIX$((max_veth_number + 1))"
    local max_ip=$(ip netns exec "$jam_namespace" ip -o addr show "$max_veth" | \
        grep -o "10\.[0-9]\+\.[0-9]\+\.[0-9]\+")
    local new_generator_ip=$(increment_ip $max_ip)
    local new_jam_ip=$(increment_ip $new_generator_ip)
    echo "$new_generator_ifname $new_generator_ip $new_jam_ip"
}

launch_generator() {
    local jam_namespace="$NAMESPACE_PREFIX:$1"
    # Exit if the jam namespace does not exist.
    ip netns list | grep -q "^$jam_namespace *" || exit 1
    local generator_namespace="$jam_namespace:$2:$3:$4"
    # Exit if the generator namespace already exists.
    ip netns list | grep -q "^$generator_namespace *" && exit 1

    # Create the namespace.
    ip netns add "$generator_namespace"
    # Set up the loopback interface.
    ip netns exec "$generator_namespace" ip link set lo up

    # Set up a veth pair between the generator namespace and the jam namespace.
    local generator_veth_name="jam.veth0"
    local jam_veth_name generator_veth_ip jam_veth_ip
    read jam_veth_name generator_veth_ip jam_veth_ip <<< $(next_veth "$1")
    ip link add "$generator_veth_name" type veth peer name "$jam_veth_name"
    ip link set "$generator_veth_name" netns "$generator_namespace"
    ip link set "$jam_veth_name" netns "$jam_namespace"
    ip netns exec "$jam_namespace" ip link set "$jam_veth_name" up
    ip netns exec "$generator_namespace" ip link set "$generator_veth_name" up
    ip netns exec "$jam_namespace" ip addr add $jam_veth_ip/31 dev "$jam_veth_name"
    ip netns exec "$generator_namespace" ip addr add "$generator_veth_ip/31" dev "$generator_veth_name"

    # Set up a tap interface in the generator namespace for the Firecracker VM.
    ip netns exec "$generator_namespace" ip tuntap add tap0 mode tap
    ip netns exec "$generator_namespace" ip link set tap0 up
    ip netns exec "$generator_namespace" ip addr add 172.16.0.1/24 dev tap0

    # Forward the Redis server port for standard access.
    ip netns exec "$generator_namespace" sysctl -w net.ipv4.ip_forward=1
    ip netns exec "$generator_namespace" iptables -t nat -A PREROUTING \
        -d 172.16.0.1 -p tcp --dport 6379 -j DNAT --to- $jam_veth_ip:6379
    ip netns exec "$generator_namespace" iptables -t nat -A POSTROUTING \
        -d $jam_veth_ip -p tcp --dport 6379 -j MASQUERADE

    # "Serve" the generator itself.
    ip netns exec "$generator_namespace" busybox nc -lp2468 < "./generators/$2:$3.tgz" &

    # Run the Firecracker VM.
    ip netns exec "$generator_namespace" /usr/local/bin/firecracker \
        --config-file /usr/local/etc/marmalade/firecracker_config.json \
        --api-sock "./jams/$1/$2/$3/$4/firecracker.sock"

    # Clean up.
    ip netns delete "$generator_namespace"
}

safely_delete_generator_namespace() {
    # Check if there are any running processes in the namespace.
    local pids="$(ip netns pids "$1")"
    if [ "$pids" ]; then
        # If there is something other than the Firecracker VM running, return an error.
        ps -ocmd= -p $pids | grep -qv "/usr/local/bin/firecracker" && \
            echo "Error: running processes in namespace $1" && return 1
        # Kill the Firecracker VM.
        kill $pids
        # Wait for the VM to die.
        sleep 1
        # Verify the namespace is clean.
        [ "$(ip netns pids "$1")" ] && \
            echo "Error: running processes in namespace $1" && return 1
    fi
    # Delete the namespace.
    ip netns delete "$1"
}

stop_jam() {
    local jam_namespace="$NAMESPACE_PREFIX:$1"
    ip netns exec "$jam_namespace" redis-cli shutdown
    [ "$(ip netns pids "$jam_namespace")" ] && \
        echo "Error: running processes in namespace $jam_namespace" && exit 1
    for netns in $(ip netns list | cut -d' ' -f1 | grep "^$jam_namespace:"); do
        safely_delete_generator_namespace "$netns" &
    done
    wait
    ip netns list | grep "^$jam_namespace:" && \
        echo "Error: failed to delete all generator namespaces connected to $jam_namespace" && exit 1
    ip netns delete "$jam_namespace"
}

[ $# -eq 0 ] && usage 1 || while [ $# -gt 0 ]; do
    case $1 in
        --help)
            usage 0
            ;;
        --start-jam|--stop-jam)
            [ $# -lt 2 ] && usage 1
            [ $1 == "--start-jam" ] && start_jam "$2" || stop_jam "$2"
            shift 2
            ;;
        --launch-generator)
            [ $# -lt 5 ] && usage 1
            launch_generator "$2" "$3" "$4" "$5"
            shift 5
            ;;
        *)
            usage 1
            ;;
    esac
done
