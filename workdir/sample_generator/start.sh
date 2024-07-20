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
tag() { redis "publish '$generator_id:tags:$1' '$2'"; }
tag "loaded at" "$(date)"
sound() { redis "publish '$generator_id:sound' '$*'"; }
for i in $(seq 2); do
    sound 'play an awesome drum break'
    tag 'drum break' 'on'
    sleep 1
    tag 'drum break' 'off'
    sleep 2
done
