/**
 * This script is for watching the whole IDE and spawning the electron process.
 *
 * It sets up watchers for the client and content, and spawns the electron process with the IDE.
 * The spawned electron process can then use its refresh capability to pull the latest changes
 * from the watchers.
 *
 * If the electron is closed, the script will restart it, allowing to test the IDE setup.
 * To stop, use Ctrl+C.
 */

import child_process from 'node:child_process'
import path from 'node:path'
import process from 'node:process'
import fs from 'node:fs/promises'

import esbuild from 'esbuild'
import * as assert from 'assert'

import * as clientBundler from './esbuild-config.js'
import * as contentBundler from '../content/esbuild-config.js'
import * as esbuildWatch from '../../esbuild-watch.js'
import { getIdeDirectory, getProjectManagerBundle, project_manager_bundle } from './paths.js'

/** Set of esbuild watches for the client and content. */
interface Watches {
    client: esbuild.BuildResult
    content: esbuild.BuildResult
}

const ideDist = getIdeDirectory()
const projectManagerBundle = getProjectManagerBundle()

console.log('Cleaning IDE dist directory.')
await fs.rm(ideDist, { recursive: true, force: true })
await fs.mkdir(ideDist, { recursive: true })

const bothBundlesReady = new Promise<Watches>(async (resolve, reject) => {
    console.log('Bundling client.')
    const clientBundlerOpts: esbuild.BuildOptions = {
        ...clientBundler.bundlerOptionsFromEnv(),
        outdir: path.resolve(ideDist),
        watch: {
            onRebuild(error, result) {
                if (error) {
                    // We cannot carry on if the client failed to build, because electron executable
                    // would immediately exit with an error.
                    console.error('Client watch bundle failed:', error)
                    reject(error)
                } else {
                    console.log('Client bundle updated.')
                }
            },
        },
    }
    let client = await esbuild.build(clientBundlerOpts)
    console.log('Result of client bundling: ', client)

    console.log('Bundling content.')
    const contentOpts = contentBundler.watchOptions(() => console.log('Content bundle updated.'))
    contentOpts.outdir = path.resolve(ideDist, 'assets')
    const content = await esbuild.build(contentOpts)
    console.log('Result of content bundling: ', content)
    resolve({ client, content })
})

const watches = await bothBundlesReady
console.log('Exposing Project Manager bundle.')
await fs.symlink(projectManagerBundle, path.join(ideDist, project_manager_bundle), 'dir')

const electronArgs = [path.join(ideDist, 'index.cjs'), '--', ...process.argv.slice(2)]

process.on('SIGINT', () => {
    console.log('SIGINT received. Exiting.')
    // The esbuild process seems to remain alive at this point and will keep our process from ending.
    // Thus, we exit manually. It seems to terminate the child esbuild process as well.
    process.exit(0)
})

while (true) {
    console.log('Spawning Electron process.')
    const electronProcess = child_process.spawn('electron', electronArgs, {
        stdio: 'inherit',
        shell: true,
    })
    console.log('Waiting for Electron process to finish.')
    const result = await new Promise((resolve, reject) => {
        electronProcess.on('close', resolve)
        electronProcess.on('error', reject)
    })
    console.log('Electron process finished.  Exit code: ', result)
}
