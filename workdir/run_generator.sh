#!/usr/bin/bash
jam="$1"
player="$2"
generator="$3"
instance="$4"
generator_archive="./generators/$player:$generator.tgz"
instance_directory="./jams/$jam/$player/$generator/$instance"
echo "Running $jam:$player:$generator:$instance"
tar -xzf "$generator_archive" -C "$instance_directory"
for file in "$instance_directory"/*; do
    if [ -x "$file" ]; then
        exec "$file"
        exit $?
    fi
done
