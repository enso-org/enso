#!/bin/bash
cd target/web

wasm-opt -O3 -o basegl_bg_opt.wasm basegl_bg.wasm
gzip --best --force basegl_bg_opt.wasm
du -h basegl_bg_opt.wasm.gz | awk '{ print $1 }'
