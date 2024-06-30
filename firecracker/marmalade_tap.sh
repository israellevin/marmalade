#!/usr/bin/bash
ip tuntap add dev tap0 mode tap
ip a
ip link delete tap0
whoami
