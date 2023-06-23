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
import * as url from 'node:url'
import process from 'node:process'

import * as esbuild from 'esbuild'

import * as clientBundler from './esbuild-config'
import * as contentBundler from '../content/esbuild-config'
import * as dashboardBundler from '../dashboard/esbuild-config'
import * as paths from './paths'

// =============
// === Types ===
// =============

/** Set of esbuild watches for the client and content. */
interface Watches {
    client: esbuild.BuildResult
    dashboard: esbuild.BuildResult
    content: esbuild.BuildResult
}

// =================
// === Constants ===
// =================

/** The path of this file. */
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
const IDE_DIR_PATH = paths.getIdeDirectory()
const PROJECT_MANAGER_BUNDLE_PATH = paths.getProjectManagerBundlePath()

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
        const dashboardOpts = dashboardBundler.bundleOptions()
        dashboardOpts.plugins.push({
            name: 'enso-on-rebuild',
            setup: build => {
                build.onEnd(() => {
                    console.log('Dashboard bundle updated.')
                })
            },
        })
        dashboardOpts.outdir = path.resolve(IDE_DIR_PATH, 'assets')
        const dashboardBuilder = await esbuild.context(dashboardOpts)
        const dashboard = await dashboardBuilder.rebuild()
        console.log('Result of dashboard bundling: ', dashboard)
        // We do not need to serve the dashboard as it outputs to the same directory.
        // It will not rebuild on request, but it is not intended to rebuild on request anyway.
        // This MUST be called before `builder.watch()` as `tailwind.css` must be generated
        // before the copy plugin runs.
        void dashboardBuilder.watch()

        console.log('Bundling content.')
        const contentOpts = contentBundler.bundlerOptionsFromEnv({
            devMode: true,
            supportsLocalBackend: true,
            supportsDeepLinks: false,
        })
        contentOpts.plugins.push({
            name: 'enso-on-rebuild',
            setup: build => {
                build.onEnd(() => {
                    console.log('Content bundle updated.')
                })
            },
        })
        contentOpts.pure.splice(contentOpts.pure.indexOf('assert'), 1)
        ;(contentOpts.inject = contentOpts.inject ?? []).push(
            path.resolve(THIS_PATH, '..', '..', 'debugGlobals.ts')
        )
        contentOpts.outdir = path.resolve(IDE_DIR_PATH, 'assets')
        contentOpts.define.REDIRECT_OVERRIDE = JSON.stringify('http://localhost:8080')
        const contentBuilder = await esbuild.context(contentOpts)
        const content = await contentBuilder.rebuild()
        console.log('Result of content bundling: ', content)
        void contentBuilder.watch()

        resolve({ client, dashboard, content })
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
