#!/usr/bin/bash
FIRECRACKER_RELEASE_BASE_URL="https://github.com/firecracker-microvm/firecracker/releases/download"
FIRECRACKER_RELEASE_URL="$FIRECRACKER_RELEASE_BASE_URL/v1.7.0/firecracker-v1.7.0-x86_64.tgz"

cd "$(dirname "${BASH_SOURCE[0]}")"

if [ ! -f generator_base.ext4 ]; then
    ./make_vm_image.sh
fi

if [ ! -x firecracker ]; then
    curl -L "$FIRECRACKER_RELEASE_URL" | tar xz --wildcards 'release-*/firecracker-*x86_64'
    find ./release-* -type f -executable -exec mv {} firecracker \;
    rm -rf release-*
fi

if [ ! -f vmlinux ]; then
    curl -L https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.9/x86_64/vmlinux-5.10.217 > vmlinux
fi

sudo sh -c 'echo 1 > /proc/sys/net/ipv4/ip_forward'

sudo ip link del tap0
sudo ip tuntap add tap0 mode tap
sudo ip link set tap0 up
sudo ip addr add 172.16.0.1/24 dev tap0

sudo iptables -F
sudo iptables -t nat -A POSTROUTING -o wlan0 -j MASQUERADE
sudo iptables -A FORWARD -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT
sudo iptables -A FORWARD -i tap0 -o wlan0 -j ACCEPT

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
      "is_read_only": true,
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
      "guest_mac": "06:00:AC:10:00:02",
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
