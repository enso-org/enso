#!/bin/bash
echo "Cleaning $1"
cat $1 | jq '.[] | select(.name | . and contains("/0x00") | not)' | jq --slurp '.' | sponge $1

