/**
 * @file This script is for watching the whole IDE and spawning the electron process.
 *
 * It sets up watchers for the client and content, and spawns the electron process with the IDE.
 * The spawned electron process can then use its refresh capability to pull the latest changes
 * from the watchers.
 *
 * If the electron is closed, the script will restart it, allowing to test the IDE setup.
 * To stop, use Ctrl+C.
 */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as process from 'node:process'

import * as esbuild from 'esbuild'

import * as clientBundler from './esbuild-config.js'
import * as contentBundler from '../content/esbuild-config.js'
import * as paths from './paths.js'

/** Set of esbuild watches for the client and content. */
interface Watches {
    client: esbuild.BuildResult
    content: esbuild.BuildResult
}

const IDE_DIR_PATH = paths.getIdeDirectory()
const PROJECT_MANAGER_BUNDLE_PATH = paths.getProjectManagerBundlePath()

console.log('Cleaning IDE dist directory.')
await fs.rm(IDE_DIR_PATH, { recursive: true, force: true })
await fs.mkdir(IDE_DIR_PATH, { recursive: true })

const BOTH_BUNDLES_READY = new Promise<Watches>((resolve, reject) => {
    void (async () => {
        console.log('Bundling client.')
        const clientBundlerOpts: esbuild.BuildOptions = {
            ...clientBundler.bundlerOptionsFromEnv(),
            outdir: path.resolve(IDE_DIR_PATH),
            watch: {
                onRebuild(error) {
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
        const client = await esbuild.build(clientBundlerOpts)
        console.log('Result of client bundling: ', client)

        console.log('Bundling content.')
        const contentOpts = contentBundler.watchOptions(() => {
            console.log('Content bundle updated.')
        })
        contentOpts.outdir = path.resolve(IDE_DIR_PATH, 'assets')
        const content = await esbuild.build(contentOpts)
        console.log('Result of content bundling: ', content)
        resolve({ client, content })
    })()
})

await BOTH_BUNDLES_READY
console.log('Exposing Project Manager bundle.')
await fs.symlink(
    PROJECT_MANAGER_BUNDLE_PATH,
    path.join(IDE_DIR_PATH, paths.PROJECT_MANAGER_BUNDLE),
    'dir'
)

const ELECTRON_ARGS = [path.join(IDE_DIR_PATH, 'index.cjs'), '--', ...process.argv.slice(2)]

process.on('SIGINT', () => {
    console.log('SIGINT received. Exiting.')
    // The esbuild process seems to remain alive at this point and will keep our process from ending.
    // Thus, we exit manually. It seems to terminate the child esbuild process as well.
    process.exit(0)
})

for (;;) {
    console.log('Spawning Electron process.')
    const electronProcess = childProcess.spawn('electron', ELECTRON_ARGS, {
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
