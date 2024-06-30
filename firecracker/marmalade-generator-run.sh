#!/bin/bash
# To run a Firecracker microVM which can run a generator, we need to 
MARMALADE_USER=i
NAMESPACE=marmalade

host_interface=$1
tap_interface=$2
ip_address=$3
firecracker_bin=$4
firecracker_socket=$5
firecracker_config=$6

sysctl -w net.ipv4.ip_forward=1
ip netns add $NAMESPACE
ip link add $NAMESPACE.veth0 type veth peer name $NAMESPACE.veth1
ip link set $NAMESPACE.veth1 netns $NAMESPACE
ip addr add 10.200.1.1/24 dev $NAMESPACE.veth0
ip link set $NAMESPACE.veth0 up
ip netns exec $NAMESPACE ip link set lo up
ip netns exec $NAMESPACE ip link set $NAMESPACE.veth1 up
ip netns exec $NAMESPACE ip addr add 10.200.1.2/24 dev $NAMESPACE.veth1
ip netns exec $NAMESPACE ip route add default via 10.200.1.1
iptables -t nat -A POSTROUTING -s 10.200.1.0/24 -o $host_interface -j MASQUERADE

ip netns exec $NAMESPACE ip tuntap add $tap_interface mode tap
ip netns exec $NAMESPACE ip link set $tap_interface up
ip netns exec $NAMESPACE ip addr add $ip_address dev $tap_interface
ip netns exec $NAMESPACE iptables -F
ip netns exec $NAMESPACE iptables -t nat -A POSTROUTING -o $NAMESPACE.veth1 -j MASQUERADE
ip netns exec $NAMESPACE iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
ip netns exec $NAMESPACE iptables -A FORWARD -i $tap_interface -o $NAMESPACE.veth1 -j ACCEPT
ip netns exec $NAMESPACE $firecracker_bin --api-sock $firecracker_socket --config-file $firecracker_config
