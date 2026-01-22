#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

YDOC_FILE="${ROOT_DIR}/app/ydoc-shared/src/uuid.ts"
GUI_TEST_DIR="${ROOT_DIR}/app/gui/src/utils/__tests__"
TEST_FILE="${GUI_TEST_DIR}/ydocSharedStale.test.ts"
BACKUP_FILE="$(mktemp "${ROOT_DIR}/.ydoc-shared-uuid.ts.bak.XXXXXX")"

cleanup() {
  if [[ -f "${BACKUP_FILE}" ]]; then
    mv -f "${BACKUP_FILE}" "${YDOC_FILE}"
  fi
  rm -f "${TEST_FILE}"
}

trap cleanup EXIT

if [[ ! -f "${YDOC_FILE}" ]]; then
  echo "Expected file not found: ${YDOC_FILE}" >&2
  exit 1
fi

if [[ ! -d "${GUI_TEST_DIR}" ]]; then
  echo "Expected test directory not found: ${GUI_TEST_DIR}" >&2
  exit 1
fi

cp "${YDOC_FILE}" "${BACKUP_FILE}"

run_step() {
  echo
  echo "==> $*"
  "$@"
}

write_test() {
  local expected="$1"
  cat >"${TEST_FILE}" <<EOF
import { expect, test } from 'vitest'
import { uuidFromBits } from 'ydoc-shared/uuid'

test('uuidFromBits renders UUID in expected case', () => {
  const uuid = uuidFromBits(0x99aabbccddeeff00n, 0x1122334455667788n)
  expect(uuid).toBe('${expected}')
})
EOF
}

update_ydoc_shared() {
  perl -0pi -e "s/padStart\(32, '0'\)/padStart(32, '0').toUpperCase()/" "${YDOC_FILE}"
}

EXPECTED_LOWER="11223344-5566-7788-99aa-bbccddeeff00"
EXPECTED_UPPER="11223344-5566-7788-99AA-BBCCDDEEFF00"

write_test "${EXPECTED_LOWER}"

run_step corepack pnpm -r compile
run_step corepack pnpm --filter enso-gui test:unit -- "${TEST_FILE}"

update_ydoc_shared
write_test "${EXPECTED_UPPER}"

set +e
corepack pnpm --filter enso-gui test:unit -- "${TEST_FILE}"
TEST_STATUS=$?
set -e

if [[ ${TEST_STATUS} -eq 0 ]]; then
  echo
  echo "Expected test failure did not occur. The stale-build issue was not reproduced." >&2
  exit 2
fi

run_step corepack pnpm -r compile
run_step corepack pnpm --filter enso-gui test:unit -- "${TEST_FILE}"

echo
echo "Reproduction succeeded: test failed before compile, passed after compile."
