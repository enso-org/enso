import { spawnSync } from 'node:child_process'
import process from 'node:process'

const isCi = process.env.GITHUB_ACTIONS === 'true' || process.env.CI === 'true'

function run(args) {
  const result = spawnSync('bazel', args, { stdio: 'inherit' })
  if (result.status !== 0) {
    process.exit(result.status ?? 1)
  }
}

if (isCi) {
  // [#14956] Windows CI has issues with stale tsconfig files, so we only generated untracked files on CI runners.
  // Additional check on Linux CI verifies that the state of committed tsconfigs matches the codebase,
  // see "check:tsconfigs" pnpm script.
  run(['run', '//:write_generated', '--verbose_failures'])
} else {
  run(['run', '//:write_all', '--verbose_failures'])
}
