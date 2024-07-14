#!/bin/sh
nc 172.16.0.1 6379 <<EOF
auth "$(head -1 ./redis.conf)" "$(tail -1 ./redis.conf)"
set 'sample generator timestamp' '$(date)'
set 'sample generator name' '$(head -1 ./redis.conf | cut -d':' -f2-)'
EOF
