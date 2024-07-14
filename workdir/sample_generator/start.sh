#!/bin/sh
generator_id=$(head -1 ./redis.conf)
password=$(tail -1 ./redis.conf)
nc 172.16.0.1 6379 <<EOF
auth $generator_id $password
set '$generator_id:loaded at' '$(date)'
EOF
