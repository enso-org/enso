#!/bin/bash
cd target/web || exit 1

cmd=$(command -v wasm-opt)
cmd_len=${#cmd}

if [ $cmd_len -eq 0 ]
then
  npm install binaryen
fi

npx wasm-opt -O3 -o basegl_bg_opt.wasm basegl_bg.wasm
gzip --best --force basegl_bg_opt.wasm
du -h basegl_bg_opt.wasm.gz | awk '{ print $1 }'
