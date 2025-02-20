/** @file Project Manager bindings. */

import { net } from 'electron'
import * as childProcess from 'node:child_process'
import * as fsSync from 'node:fs'
import * as url from 'node:url'
import * as util from 'node:util'

import * as contentConfig from '@/contentConfig'

import type * as config from '@/config'
import { getProjectRoot } from './projectManagement'

const logger = contentConfig.logger
const execFile = util.promisify(childProcess.execFile)

// =======================
// === Project Manager ===
// =======================

/**
 * Return the Project Manager path.
 * @throws If the Project Manager path is invalid.
 */
export function pathOrPanic(args: config.Args): string {
  const binPath = args.groups.engine.options.projectManagerPath.value
  const binExists = fsSync.existsSync(binPath)
  if (!binExists) {
    throw new Error(`Could not find the project manager binary at ${binPath}.`)
  } else {
    return binPath
  }
}

/** Execute the Project Manager with given arguments. */
async function exec(args: config.Args, processArgs: string[], env?: NodeJS.ProcessEnv) {
  const binPath = pathOrPanic(args)
  return await execFile(binPath, processArgs, { env })
}

/**
 * Spawn the Project Manager process.
 *
 * The standard output and error handles will be redirected to the output and error handles of the
 * Electron app. Input is piped to this process, so it will not be closed until this process
 * finishes.
 */
export function spawn(
  args: config.Args,
  processArgs: string[],
  env?: NodeJS.ProcessEnv,
): childProcess.ChildProcess {
  return logger.groupMeasured(
    `Starting the backend process with the following options: ${processArgs.join(', ')}.`,
    () => {
      const binPath = pathOrPanic(args)
      const process = childProcess.spawn(binPath, processArgs, {
        stdio: [/* stdin */ 'pipe', /* stdout */ 'inherit', /* stderr */ 'inherit'],
        env,
        // The Project Manager should never spawn any windows. On Windows OS this needs
        // to be manually prevented, as the default is to spawn a console window.
        windowsHide: true,
      })
      logger.log(`Backend has been spawned (pid = ${String(process.pid)}).`)
      process.on('exit', (code) => {
        logger.log(`Backend exited with code ${String(code)}.`)
      })
      return process
    },
  )
}

/** Run an arbitrary command and return its output. */
export function runCommand(
  args: config.Args,
  processArgs: string[],
  body?: NodeJS.ReadableStream,
  env?: NodeJS.ProcessEnv,
) {
  const binPath = pathOrPanic(args)
  const process = childProcess.spawn(binPath, processArgs, {
    stdio: [/* stdin */ 'pipe', /* stdout */ 'pipe', /* stderr */ 'ignore'],
    env,
    // The Project Manager should never spawn any windows. On Windows OS this needs
    // to be manually prevented, as the default is to spawn a console window.
    windowsHide: true,
  })
  if (body != null) {
    body.pipe(process.stdin, { end: true })
  } else {
    process.stdin.end()
  }
  return process.stdout
}

/** Get the Project Manager version. */
export async function version(args: config.Args) {
  if (args.options.engine.value) {
    return await exec(args, ['--version']).then((t) => t.stdout)
  } else {
    return
  }
}

/**
 * Handle requests to the `enso://` protocol.
 *
 * The protocol is used to fetch project assets from the backend.
 * If a given path is not inside a project, the request is rejected with a 403 error.
 */
export async function handleProjectProtocol(absolutePath: string) {
  if (getProjectRoot(absolutePath) == null) {
    logger.error(`The given path is not inside a project: ${absolutePath}.`)
    return new Response(null, { status: 403 })
  }

  return net.fetch(url.pathToFileURL(absolutePath).toString())
}
