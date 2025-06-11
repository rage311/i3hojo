#!/bin/sh
df -h --output=avail "$1" | tail -1 | cut -d' ' -f2
