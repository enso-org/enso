import { spawnSync } from 'node:child_process'
import process from 'node:process'

function run(command, args, options = {}) {
  const result = spawnSync(command, args, {
    stdio: options.captureOutput ? 'pipe' : 'inherit',
    encoding: options.captureOutput ? 'utf8' : undefined,
  })
  if (result.status !== 0) {
    process.exit(result.status ?? 1)
  }
  return result.stdout ?? ''
}

run('bazel', ['run', '//:write_all', '--verbose_failures'])

// There is an issue on Windows that causes changes in MODULE.bazel.lock.
const exclude = [':(exclude)MODULE.bazel.lock', ':(exclude)app/gui/.dev-env']

const diff = run('git', ['diff', '--stat', '--', '.', ...exclude], { captureOutput: true })

if (diff.trim() !== '') {
  console.error('Dirty files after running //:write_all :')
  console.error(diff.trimEnd())
  process.exit(1)
}
