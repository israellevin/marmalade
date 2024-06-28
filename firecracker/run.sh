#!/usr/bin/bash

if [ "$1" != "--slirped" ]; then
    touch /tmp/marmalade.netns
    (sleep 0.1; slirp4netns --configure --mtu=65520 --netns-type=path /tmp/marmalade.netns slirp) &
    slirp_pid=$!
    unshare --net=/tmp/marmalade.netns --user --mount --map-root-user -- "$0" --slirped "$@"
    kill $slirp_pid
    umount /tmp/marmalade.netns
    exit 0
fi
shift

sh -c 'echo 1 > /proc/sys/net/ipv4/ip_forward'
ip tuntap add tap0 mode tap
ip link set tap0 up
ip addr add 172.16.0.1/24 dev tap0
iptables -F
iptables -t nat -A POSTROUTING -o slirp -j MASQUERADE
iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
iptables -A FORWARD -i tap0 -o slirp -j ACCEPT

cd "$(dirname "${BASH_SOURCE[0]}")"

rm /tmp/firecracker.socket
./firecracker --api-sock /tmp/firecracker.socket --config-file <(cat <<EOF
{
  "boot-source": {
    "kernel_image_path": "./vmlinux",
    "boot_args": "console=ttyS0 reboot=k panic=1 pci=off",
    "initrd_path": null
  },
  "drives": [
    {
      "drive_id": "rootfs",
      "partuuid": null,
      "is_root_device": true,
      "cache_type": "Unsafe",
      "is_read_only": false,
      "path_on_host": "./generator_base.ext4",
      "io_engine": "Sync",
      "rate_limiter": null,
      "socket": null
    }
  ],
  "machine-config": {
    "vcpu_count": 2,
    "mem_size_mib": 1024,
    "smt": false,
    "track_dirty_pages": false,
    "huge_pages": "None"
  },
  "cpu-config": null,
  "balloon": null,
  "network-interfaces": [
    {
      "iface_id": "eth0",
      "guest_mac": "AA:FC:00:00:00:01",
      "host_dev_name": "tap0"
    }
  ],
  "vsock": null,
  "logger": null,
  "metrics": null,
  "mmds-config": null,
  "entropy": null
}
EOF
)
