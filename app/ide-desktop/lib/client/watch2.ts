/** @file This script is for watching the whole IDE and spawning the electron process.
 *
 * It sets up watchers for the client and content, and spawns the electron process with the IDE.
 * The spawned electron process can then use its refresh capability to pull the latest changes
 * from the watchers.
 *
 * If the electron app is closed, the script will restart it, allowing to test the IDE setup.
 * To stop, use Ctrl+C. */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import process from 'node:process'

import * as esbuild from 'esbuild'

import * as clientBundler from './esbuild-config'
import * as paths from './paths'

// =============
// === Types ===
// =============

/** Set of esbuild watches for the client and content. */
interface Watches {
    readonly client: esbuild.BuildResult
    readonly dashboard: esbuild.BuildResult
}

// =================
// === Constants ===
// =================

const IDE_DIR_PATH = paths.getIdeDirectory()
const PROJECT_MANAGER_BUNDLE_PATH = paths.getProjectManagerBundlePath()
        const APP_OUT_DIR = path.resolve(IDE_DIR_PATH, 'assets')

// =============
// === Watch ===
// =============

console.log('Cleaning IDE dist directory.')
await fs.rm(IDE_DIR_PATH, { recursive: true, force: true })
await fs.mkdir(IDE_DIR_PATH, { recursive: true })

const ALL_BUNDLES_READY = new Promise<Watches>((resolve, reject) => {
    void (async () => {
        console.log('Bundling client.')
        const clientBundlerOpts = clientBundler.bundlerOptionsFromEnv()
        clientBundlerOpts.outdir = path.resolve(IDE_DIR_PATH)
        // Eslint is wrong here; `clientBundlerOpts.plugins` is actually `undefined`.
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        ;(clientBundlerOpts.plugins ??= []).push({
            name: 'enso-on-rebuild',
            setup: build => {
                build.onEnd(result => {
                    if (result.errors.length) {
                        // We cannot carry on if the client failed to build, because electron
                        // would immediately exit with an error.
                        console.error('Client watch bundle failed:', result.errors[0])
                        reject(result.errors[0])
                    } else {
                        console.log('Client bundle updated.')
                    }
                })
            },
        })
        const clientBuilder = await esbuild.context(clientBundlerOpts)
        const client = await clientBuilder.rebuild()
        console.log('Result of client bundling: ', client)
        void clientBuilder.watch()

        console.log('Bundling dashboard.')
        childProcess.spawn('vite', ['build', '../../../gui2/', '-c', '../../../gui2/vite.watch.config.ts', '--watch', '--outDir', APP_OUT_DIR], {stdio: 'inherit', 
            shell: true,
        })

        resolve({ client })
    })()
})

await ALL_BUNDLES_READY
console.log('Exposing Project Manager bundle.')
await fs.symlink(
    PROJECT_MANAGER_BUNDLE_PATH,
    path.join(IDE_DIR_PATH, paths.PROJECT_MANAGER_BUNDLE),
    'dir'
)

const ELECTRON_ARGS = [path.join(IDE_DIR_PATH, 'index.cjs'), '--', ...process.argv.slice(2)]

process.on('SIGINT', () => {
    console.log('SIGINT received. Exiting.')
    // The `esbuild` process seems to remain alive at this point and will keep our process
    // from ending. Thus, we exit manually. It seems to terminate the child `esbuild` process
    // as well.
    process.exit(0)
})

console.log('Waiting for Vite to finish initial build...')
await new Promise((resolve) => {
    const indexHtml = path.join(APP_OUT_DIR, 'index.html')
    setInterval(() => {
            fs.stat(indexHtml).then(resolve, () => {})
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    }, 500)
})
console.log('Vite buil finished.')

while (true) {
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
