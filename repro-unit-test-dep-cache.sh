#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$repo_root"

echo "Repo root: $repo_root"

if [[ ! -f .node-version ]]; then
  echo "Missing .node-version in repo root." >&2
  exit 1
fi

echo "Using node version from .node-version: $(cat .node-version)"
fnm use "$(cat .node-version)"
echo "Node: $(node -v)"
echo "Pnpm: $(corepack pnpm --version)"

scenario="${1:-}"
if [[ -z "$scenario" ]]; then
  echo "Usage: $0 <scenario>" >&2
  echo "Scenarios: ydoc-shared gui-ast enso-common-pm-shim ydoc-shared-ydoc-server enso-common-ydoc-shared enso-common-ydoc-server" >&2
  exit 1
fi

case "$scenario" in
  ydoc-shared)
    dep_file="app/ydoc-shared/src/ast/index.ts"
    test_file="app/gui/src/project-view/util/ast/__tests__/repro-ydoc-shared-cache.test.ts"
    test_import="ydoc-shared/ast"
    test_command=(corepack pnpm run --filter enso-gui test:unit)
    build_command=(corepack pnpm run build:gui)
    ;;
  gui-ast)
    dep_file="app/gui/src/project-view/util/ast/index.ts"
    test_file="app/gui/src/project-view/util/ast/__tests__/repro-gui-ast-cache.test.ts"
    test_import="@/util/ast"
    test_command=(corepack pnpm run --filter enso-gui test:unit)
    build_command=(corepack pnpm run build:gui)
    ;;
  enso-common-pm-shim)
    dep_file="app/common/src/utilities/errors.ts"
    test_file="app/project-manager-shim/src/__tests__/repro-enso-common-cache.test.ts"
    test_import="enso-common/src/utilities/errors"
    test_command=(corepack pnpm --filter project-manager-shim exec vitest run src/__tests__/repro-enso-common-cache.test.ts)
    build_command=(corepack pnpm run -r compile)
    ;;
  ydoc-shared-ydoc-server)
    dep_file="app/ydoc-shared/src/ast/index.ts"
    test_file="app/ydoc-server/src/__tests__/repro-ydoc-shared-cache.test.ts"
    test_import="ydoc-shared/ast"
    test_command=(corepack pnpm --filter ydoc-server exec vitest run src/__tests__/repro-ydoc-shared-cache.test.ts)
    build_command=(corepack pnpm run -r compile)
    ;;
  enso-common-ydoc-shared)
    dep_file="app/common/src/utilities/errors.ts"
    test_file="app/ydoc-shared/src/__tests__/repro-enso-common-cache.test.ts"
    test_import="enso-common/src/utilities/errors"
    test_command=(corepack pnpm --filter ydoc-shared exec vitest run src/__tests__/repro-enso-common-cache.test.ts)
    build_command=(corepack pnpm run -r compile)
    ;;
  enso-common-ydoc-server)
    dep_file="app/common/src/utilities/errors.ts"
    test_file="app/ydoc-server/src/__tests__/repro-enso-common-cache.test.ts"
    test_import="enso-common/src/utilities/errors"
    test_command=(corepack pnpm --filter ydoc-server exec vitest run src/__tests__/repro-enso-common-cache.test.ts)
    build_command=(corepack pnpm run -r compile)
    ;;
  *)
    echo "Unknown scenario: $scenario" >&2
    echo "Scenarios: ydoc-shared gui-ast enso-common-pm-shim ydoc-shared-ydoc-server enso-common-ydoc-shared enso-common-ydoc-server" >&2
    exit 1
    ;;
esac

if [[ ! -f "$dep_file" ]]; then
  echo "Missing expected file: $dep_file" >&2
  exit 1
fi

tmp_dir="$(mktemp -d)"
backup_file="$tmp_dir/dependency.ts"
cleanup() {
  echo "Cleaning up temp files"
  if [[ -f "$backup_file" ]]; then
    cp "$backup_file" "$dep_file"
  fi
  if [[ -f "$test_file" ]]; then
    rm -f "$test_file"
  fi
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cp "$dep_file" "$backup_file"
mkdir -p "$(dirname "$test_file")"

printf '\nexport class ReproCacheMarker extends Error {}\n' >> "$dep_file"
echo "Appended export to $dep_file"

cat <<EOF > "$test_file"
import { describe, expect, it } from "vitest"
import * as Dep from "${test_import}"

describe("dependency cache repro (${scenario})", () => {
  it("sees the new export", () => {
    const marker = (Dep as Record<string, unknown>).ReproCacheMarker as new (message: string) => Error
    const error = new marker("repro")
    expect(error).toBeInstanceOf(marker)
  })
})
EOF

echo "Created repro test at $test_file"

logs_dir="$repo_root/.repro-logs"
mkdir -p "$logs_dir"
log_prefix="$logs_dir/${scenario}"

echo "Scenario: $scenario"
echo "Dependency file: $dep_file"
echo "Test file: $test_file"
echo "Running scenario unit tests without build step"
set +e
"${test_command[@]}" > "$log_prefix.before.log" 2>&1
first_status=$?
set -e

if [[ $first_status -eq 0 ]]; then
  echo "Scenario unit tests passed without build step." >&2
else
  echo "Scenario unit tests failed without build step." >&2
fi

echo "Running build step: ${build_command[*]}"
"${build_command[@]}"

echo "Running scenario unit tests after build step"
set +e
"${test_command[@]}" > "$log_prefix.after.log" 2>&1
second_status=$?
set -e

if [[ $first_status -ne 0 ]]; then
  echo "Unit test failures before build step (showing last 200 lines):" >&2
  tail -n 200 "$log_prefix.before.log" >&2
fi

if [[ $second_status -ne 0 ]]; then
  echo "Unit test failures after build step (showing last 200 lines):" >&2
  tail -n 200 "$log_prefix.after.log" >&2
fi

if [[ $second_status -eq 0 ]]; then
  echo "Scenario unit tests passed after build step." >&2
else
  echo "Scenario unit tests still failing after build step." >&2
fi

if [[ $first_status -ne 0 && $second_status -eq 0 ]]; then
  echo "Reproduced"
else
  echo "Didn't reproduce"
fi

exit 0
