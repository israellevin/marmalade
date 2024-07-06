#!/usr/bin/bash
cd "$(dirname "${BASH_SOURCE[0]}")"
rm /tmp/firecracker.socket

ip netns delete marmalade
ip netns add marmalade
ip netns exec marmalade ip tuntap add tap0 mode tap
ip netns exec marmalade ip link set tap0 up
ip netns exec marmalade ip addr add 172.16.0.1/24 dev tap0
ip netns exec marmalade ./firecracker --api-sock /tmp/firecracker.socket --config-file <(cat <<EOF
#marmalade-generator-run wlan0 tap0 172.16.0.1/24 ./firecracker /tmp/firecracker.socket <(cat <<EOF
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
