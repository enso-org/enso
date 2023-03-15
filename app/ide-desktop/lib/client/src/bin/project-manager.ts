/** @file Project Manager bindings. */

import * as childProcess from 'node:child_process'
import * as fsSync from 'node:fs'
import * as util from 'node:util'

import * as contentConfig from 'enso-content-config'

import * as config from 'config'

// This is a wrapped function, so it should be `camelCase`.
// eslint-disable-next-line no-restricted-syntax
const execFile = util.promisify(childProcess.execFile)

// =======================
// === Project Manager ===
// =======================

/** Return the Project Manager path.
 * @throws If the Project Manager path is invalid. */
export function pathOrPanic(args: config.Args): string {
    const binPath = args.groups.engine.options.projectManagerPath.value
    const binExists = fsSync.existsSync(binPath)
    if (!binExists) {
        throw new Error(`Could not find the project manager binary at ${binPath}.`)
    }
    return binPath
}

/** Executes the Project Manager with given arguments. */
async function exec(args: config.Args, processArgs: string[]) {
    const binPath = pathOrPanic(args)
    return await execFile(binPath, processArgs).catch(function (err) {
        throw err
    })
}

/** Spawn Project Manager process.
 *
 * The standard output and error handles will be redirected to the electron's app output and error
 * handles. Input is piped to this process, so it will not be closed, until this process
 * finished. */
export function spawn(args: config.Args, processArgs: string[]): childProcess.ChildProcess {
    return contentConfig.LOGGER.groupMeasured(
        `Starting the backend process with the following options: ${processArgs.join(', ')}.`,
        () => {
            const binPath = pathOrPanic(args)
            const process = childProcess.spawn(binPath, processArgs, {
                stdio: [/* stdin */ 'pipe', /* stdout */ 'inherit', /* stderr */ 'inherit'],
            })
            contentConfig.LOGGER.log(`Backend has been spawned (pid = ${String(process.pid)}).`)
            process.on('exit', code => {
                contentConfig.LOGGER.log(`Backend exited with code ${String(code)}.`)
            })
            return process
        }
    )
}

/** Get the Project Manager version. */
export async function version(args: config.Args) {
    if (args.options.engine.value) {
        return await exec(args, ['--version']).then(t => t.stdout)
    }
}
