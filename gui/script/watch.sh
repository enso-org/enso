#!/bin/bash
cargo watch -i .gitignore -i "pkg/*" -s "script/build.sh ${@}"