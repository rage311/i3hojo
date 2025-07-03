#!/bin/sh
df -h --output=avail "$1" | awk -e 'END { print $1 }'
