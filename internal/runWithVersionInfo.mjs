import { spawn } from 'node:child_process'
import fs from 'node:fs/promises'
import process from 'node:process'

if (process.env.JS_BINARY__EXECROOT) {
  process.chdir(process.env.JS_BINARY__EXECROOT)
}

function usage() {
  throw new Error(
    `Invalid arguments.\nusage:\n  ${process.argv[0]} ${process.argv[1]} [--status-file <stable-status.txt>] <executable> <args...>`,
  )
}

/**
 * Parse Bazel stable-status.txt into a key/value map.
 * Keys are returned without the leading 'STABLE_' prefix.
 *
 * @param {string} content
 * @returns {Record<string, string>}
 */
function parseStableStatus(content) {
  /** @type {Record<string, string>} */
  const vars = {}
  for (const line of content.split(/\r?\n/)) {
    const [key, value] = line.split(' ', 2)
    if (key && key.startsWith('STABLE_ENSO')) {
      const envName = key.slice('STABLE_'.length)
      vars[envName] = value
    }
  }
  return vars
}

function spawnExecutable(executable, args, env) {
  const child = spawn(executable, args, {
    stdio: 'inherit',
    env,
  })

  child.on('exit', (code, signal) => {
    if (signal) {
      process.kill(process.pid, signal)
      return
    }
    process.exit(code ?? 1)
  })
  child.on('error', (err) => {
    console.error(err)
    process.exit(1)
  })
}

let idx = 2
let statusFilePath
if (process.argv[idx] === '--status-file') {
  statusFilePath = process.argv[idx + 1]
  idx += 2
}

if (!process.argv[idx]) usage()
const executable = process.argv[idx]
const execArgs = process.argv.slice(idx + 1)

if (statusFilePath != null) {
  const content = await fs.readFile(statusFilePath, 'utf8')
  const stableVars = parseStableStatus(content)

  // TODO: sbt should really use ENSO_IDE_VERSION and ENSO_IDE_EDITION variables.
  // Or, IDE build should use more universal ENSO_VERSION and ENSO_EDITION
  const ensoVersion = stableVars['ENSO_IDE_VERSION']
  const ensoEdition = stableVars['ENSO_IDE_EDITION']

  const childEnv = {
    ...process.env,
    ...stableVars,
    ENSO_VERSION: ensoVersion,
    ENSO_EDITION: ensoEdition,
    ENSO_RELEASE_MODE: 'true',
  }

  spawnExecutable(executable, execArgs, childEnv)
} else {
  spawnExecutable(executable, execArgs, process.env)
}
