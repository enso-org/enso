#!/bin/sh

# USAGE: ./tools/build-performance/bench-build.sh ~/benchmarks/output_file.csv
#
# Measures time to build after making changes to the sources.
#
# Notes:
# - This command will run `git clean`. Uncommitted work may be lost!
# - Must be run from root directory of repo.
# - The `time` command must support certain non-standard options. The version that ships with Ubuntu 21.10 works.

set -e

./run.sh clean

OUT="$1"

bench_mode() {
  MODE="$1"

  $(which time) -f "${MODE},%e" -o "${OUT}" -a -- ./run.sh wasm build --wasm-profile="${MODE}"

  # Ensure any incremental files are loaded into memory.
  # In my[KW] tests, despite having 64GB RAM, this makes a factor of 2 difference to the dev_change_nothing time.
  ./run.sh wasm build --wasm-profile="${MODE}"

  $(which time) -f "${MODE}_change_nothing,%e" -o "${OUT}" -a -- ./run.sh wasm build --wasm-profile="${MODE}"

  touch -c app/gui/src/lib.rs
  $(which time) -f "${MODE}_change_ide,%e" -o "${OUT}" -a -- ./run.sh wasm build --wasm-profile="${MODE}"

  touch -c app/gui/view/graph-editor/src/lib.rs
  $(which time) -f "${MODE}_change_grapheditor,%e" -o "${OUT}" -a -- ./run.sh wasm build --wasm-profile="${MODE}"

  touch -c lib/rust/prelude/src/lib.rs
  $(which time) -f "${MODE}_change_prelude,%e" -o "${OUT}" -a -- ./run.sh wasm build --wasm-profile="${MODE}"
}

bench_mode dev
bench_mode release
