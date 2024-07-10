#!/usr/bin/bash -e
BUILD_DIR="$1"
IMAGE=alpine
TAG=latest
IMAGE_URL="https://auth.docker.io/token?service=registry.docker.io&scope=repository:library/$IMAGE:pull"
MANIFEST_URL="https://registry-1.docker.io/v2/library/$IMAGE/manifests/$TAG"
FIRECRACKER_RELEASE_BASE_URL="https://github.com/firecracker-microvm/firecracker/releases/download"
FIRECRACKER_RELEASE_URL="$FIRECRACKER_RELEASE_BASE_URL/v1.7.0/firecracker-v1.7.0-x86_64.tgz"

get_firecracker() {
    [ -x firecracker ] && echo Firecracker binary found - skipping download && return
    echo Downloading Firecracker binary
    curl -fL "$FIRECRACKER_RELEASE_URL" | tar xz --wildcards 'release-*/firecracker-*x86_64'
    find ./release-* -type f -executable -exec mv {} firecracker \;
    rm -rf release-*
}

get_vmlinux() {
    [ -f vmlinux ] && echo Linux kernel for Firecracker found - skipping download && return
    echo Downloading kernel for Firecracker
    curl -fL https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.9/x86_64/vmlinux-6.1.96 -o vmlinux
}

make_rootfs_directory() {
    local rootfs_dir="$1"
    [ -d "$rootfs_dir" ] && echo Rootfs directory "'$rootfs_dir'" found - skipping rootfs directory creation && return
    echo Downloading and extracting rootfs in "'$rootfs_dir'"
    token=$(curl -sfL "$IMAGE_URL" | jq -r .token)
    mkdir "$rootfs_dir"
    curl -sfL "$MANIFEST_URL" \
        -H "Authorization: Bearer $token" \
        -H 'Accept: application/vnd.docker.distribution.manifest.v2+json' | jq -r '.layers[].digest' | \
            while read digest; do
                curl -fLH "Authorization: Bearer $token" \
                    "https://registry-1.docker.io/v2/library/$IMAGE/blobs/$digest" | tar -xzC "$rootfs_dir"/
            done

    echo Installing base generator system in "'$rootfs_dir'"
    sudo chroot "$rootfs_dir" /bin/sh <<'EOF'
echo nameserver 1.1.1.1 > /etc/resolv.conf
chmod 644 /etc/resolv.conf
apk add openrc
apk add util-linux
ln -s agetty /etc/init.d/agetty.ttyS0
echo ttyS0 > /etc/securetty
rc-update add agetty.ttyS0 default
rc-update add devfs boot
rc-update add local boot
rc-update add procfs boot
rc-update add sysfs boot
mkdir /generator/
touch /etc/local.d/marmalade.start
chmod 755 /etc/local.d/marmalade.start
passwd -d root
EOF
    sudo chown -R "$USER:$USER" "$rootfs_dir"
    cat > "$rootfs_dir/etc/local.d/marmalade.start" <<'EOF'
#!/bin/sh -e
mount -t tmpfs none /generator
exec > /generator/setup.log 2>&1
ip addr add 172.16.0.2/24 dev eth0
ip link set eth0 up
ip route add default via 172.16.0.1 dev eth0
echo ok | nc 172.16.0.1 2468 | tar -xzC /generator
cd /generator
for file in *; do
    if [ -x "$file" ]; then
        "./$file"
        reboot
    fi
done
EOF
}

make_rootfs_image() {
    [ -f generator_base.ext4 ] && echo Image file found - skipping image creation && return
    local rootfs_dir="${1:-rootfs}"
    [ -d "$rootfs_dir" ] || make_rootfs_directory "$rootfs_dir" && remove_rootfs_directory=1
    echo Writing directory "'$rootfs_dir'" to image
    rootfs_size=$(du -sB1 "$rootfs_dir" | cut -f1)
    initial_image_size=$((rootfs_size))
    dd if=/dev/zero of=generator_base.ext4 bs=1 count=0 seek=$initial_image_size
    /usr/sbin/mkfs.ext4 -d "$rootfs_dir" generator_base.ext4
    /usr/sbin/resize2fs -M generator_base.ext4
    [ "$remove_rootfs_directory" ] && rm -rf "$rootfs_dir"
}

# If the script is sourced, return without executing.
[ "$BASH_SOURCE" == "$0" ] || return 0

set -e
get_firecracker
get_vmlinux
make_rootfs_image
