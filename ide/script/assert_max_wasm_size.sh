#!/bin/bash

echoerr() { echo "$@" 1>&2; }

current_size=$(cat file_size)
current_size_len=$((${#current_size} - 1))
current_size="${current_size:0:current_size_len}"

max_size=2.1 # MB
echo "Current size: ${current_size}MiB. Expected maximum size: ${max_size}MiB"
if (( $(echo "$current_size <= $max_size" |bc -l) ));
then
  echo OK
else
  echoerr FAIL
  exit 1
fi
