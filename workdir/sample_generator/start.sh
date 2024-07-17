#!/bin/sh

cleanup() {
    echo shutdown > redis-in
    kill $nc_pid
    kill $cat_pid
    exec 3>&-
    wait
    rm -f redis-in redis-pipe redis-out
}
trap cleanup EXIT ERR

generator_id=$(head -1 ./redis.auth)
password=$(tail -1 ./redis.auth)
mkfifo redis-in redis-pipe
nc 172.16.0.1 6379 < redis-in > redis-pipe &
nc_pid=$!
cat redis-pipe > redis-out &
cat_pid=$!
exec 3>redis-in
redis() { echo "$*" > redis-in && tail -1 redis-out; }
redis "auth $generator_id $password"

tag() { redis "set '$generator_id:$1' '$2'"; }
tag "loaded at" "$(date)"
