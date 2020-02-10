#!/bin/bash
cd target/web || exit 1

cmd=$(command -v wasm-opt)
cmd_len=${#cmd}

if [ $cmd_len -eq 0 ]
then
  npm install binaryen
fi

npx wasm-opt -O3 -o gui_opt.wasm gui.wasm
gzip --best --force gui_opt.wasm
du -h gui_opt.wasm.gz | awk '{ print $1 }'
