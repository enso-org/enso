import child_process, { SpawnOptions } from 'child_process'
import * as config from 'config'
import fss from 'node:fs'
import { logger } from 'enso-content-config'
import util from 'node:util'
const execFile = util.promisify(child_process.execFile)

// =======================
// === Project Manager ===
// =======================

/** Return the Project Manager path if it is valid. Otherwise, throw an error. */
export function pathOrPanic(args: config.Args): string {
    let binPath = args.groups.engine.options.projectManagerPath.value
    let binExists = fss.existsSync(binPath)
    if (!binExists) {
        throw new Error(`Could not find the project manager binary at ${binPath}.`)
    }
    return binPath
}

/** Executes the Project Manager with given arguments. */
async function exec(args: config.Args, processArgs: string[]) {
    let binPath = pathOrPanic(args)
    return await execFile(binPath, processArgs).catch(function (err) {
        throw err
    })
}

/** Spawn Project Manager process.
 *
 * The standard output and error handles will be redirected to the electron's app output and error
 * handles. Input is piped to this process, so it will not be closed, until this process
 * finished. */
export function spawn(args: config.Args, processArgs: string[]): child_process.ChildProcess {
    return logger.groupMeasured(
        `Starting the backend process with the following options: ${processArgs}`,
        () => {
            const binPath = pathOrPanic(args)
            const stdin = 'pipe' as const
            const stdout = 'inherit' as const
            const stderr = 'inherit' as const
            const stdio = [stdin, stdout, stderr]
            const process = child_process.spawn(binPath, processArgs, { stdio })
            logger.log(`Backend has been spawned (pid = ${process.pid}).`)
            process.on('exit', code => logger.log(`Backend exited with code ${code}.`))
            return process
        }
    )
}

export async function version(args: config.Args) {
    if (args.options.engine.value) {
        return await exec(args, ['--version']).then(t => t.stdout)
    }
}
