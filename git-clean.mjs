/**
 * @file Clean workspace of all temporary files and directories.
 * Calls `git clean -xdf` with some exclusions, and also `bazel clean --expunge`, to clean Bazel build artifacts.
 */

const EXCLUSIONS = [
  '.idea',
  '.jj',
  '.bazelrc.local',
  'app/gui/',
  'app/electron-client/playwright/.auth/user.json',
]

import { spawn } from 'node:child_process'
import { once } from 'node:events'
import fs from 'node:fs/promises'
import process from 'node:process'

const VERBOSE = process.argv.includes('--verbose') || process.argv.includes('-v')
const CLEAN_BAZEL = process.argv.includes('--clean-bazel')

async function runCommand(command, args) {
  const child = spawn(command, args, { stdio: 'pipe' })
  let stdout = ''
  let stderr = ''

  child.stdout.on('data', (data) => (stdout += data))
  child.stderr.on('data', (data) => (stderr += data))

  if (VERBOSE) {
    console.log(`Running command: ${command} ${args.join(' ')}`)
  }
  const [code] = await once(child, 'close')

  if (code !== 0) {
    console.error(`Failed to run command ${command}: ${stderr}`)
    process.exit(1)
  }

  if (VERBOSE) {
    console.log('STDOUT:')
    console.log(stdout)
    console.log('STDERR:')
    console.log(stderr)
  }
}

async function runGitClean() {
  return runCommand('git', ['clean', '-xdf', ...EXCLUSIONS.flatMap((e) => ['--exclude', e])])
}

async function runBazelClean() {
  let executable = 'bazel'
  return runCommand(executable, ['clean', '--expunge_async'])
}

async function removeIfExists(path) {
  try {
    await fs.rm(path, { recursive: true, force: true })
    if (VERBOSE) {
      console.log(`Removed: ${path}`)
    }
  } catch (err) {
    if (err.code !== 'ENOENT') {
      console.error(`Failed to remove ${path}: ${err.message}`)
      throw err
    }
    // If error is ENOENT, entry does not exist, ignore
  }
}

async function cleanJunctions() {
  // On Windows, `pnpm` uses junctions as symbolic links for in-workspace dependencies.
  // Unfortunately, Git for Windows treats those as hard links. That then leads to
  // `git clean` recursing into those linked directories, happily deleting sources of
  // whole linked packages or failing on files that were already deleted. Manually
  // deleting junction directories before running clean prevents this from happening.
  let junctions = [
    'bazel-enso',
    'bazel-out',
    'bazel-bin',
    'node_modules',
    'app/common/node_modules',
    'app/gui/node_modules',
    'app/electron-client/node_modules',
    'app/lang-markdown/node_modules',
    'app/lezer-markdown/node_modules',
    'app/project-manager-shim/node_modules',
    'app/rust-ffi/node_modules',
    'app/table-expression/node_modules',
    'app/ydoc-server/node_modules',
    'app/ydoc-server-nodejs/node_modules',
    'app/ydoc-server-polyglot/node_modules',
    'app/ydoc-shared/node_modules',
    'lib/js/runner/node_modules',
    'tools/simple-library-server/node_modules',
  ]

  await Promise.all(junctions.map((junction) => removeIfExists(junction)))
}

if (CLEAN_BAZEL) {
  await runBazelClean()
}
if (process.platform === 'win32') {
  await cleanJunctions()
}
await runGitClean()
