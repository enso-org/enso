import childProcess from 'node:child_process'

import { redactArgs, sanitizeError } from './sanitize.mjs'

export function run(command, args, options = {}) {
  const {
    cwd,
    env,
    redactValues = [],
    verbose = false,
    maxBuffer = 64 * 1024 * 1024,
    stdio,
  } = options
  const resolvedStdio = stdio ?? (verbose ? 'inherit' : 'pipe')

  if (verbose) {
    console.log('Running', command, redactArgs(args, redactValues), cwd)
  }

  try {
    if (resolvedStdio === 'inherit') {
      const result = childProcess.spawnSync(command, args, {
        cwd,
        env,
        encoding: 'utf8',
        stdio: 'inherit',
      })

      if (result.error) {
        throw result.error
      }
      if (result.status !== 0) {
        const error = new Error(
          `Command failed with exit code ${result.status}: ${command} ${redactArgs(args, redactValues).join(' ')}`,
        )
        error.status = result.status
        error.signal = result.signal
        throw error
      }

      return ''
    }

    return childProcess.execFileSync(command, args, {
      cwd,
      env,
      encoding: 'utf8',
      stdio: ['ignore', resolvedStdio, resolvedStdio],
      maxBuffer,
    })
  } catch (error) {
    throw sanitizeError(error, redactValues)
  }
}
