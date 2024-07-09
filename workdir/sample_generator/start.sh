#!/bin/sh
echo "set 'sample generator timestamp' '$(date)'" | nc 172.16.0.1 6379
