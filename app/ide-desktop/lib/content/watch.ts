/** @file File watch and compile service. */
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import * as portfinder from 'portfinder'
import chalk from 'chalk'

import * as bundler from './esbuild-config'
import * as dashboardBundler from '../dashboard/esbuild-config'

// =================
// === Constants ===
// =================

/** The path of this file. */
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
const PORT = 8080
const HTTP_STATUS_OK = 200

// ===============
// === Watcher ===
// ===============

/** Starts the esbuild watcher. */
async function watch() {
    const dashboardOpts = dashboardBundler.bundleOptions()
    const dashboardBuilder = await esbuild.context(dashboardOpts)
    // We do not need to serve the dashboard as it outputs to the same directory.
    // It will not rebuild on request, but it is not intended to rebuild on request anyway.
    // This MUST be called before `builder.watch()` as `tailwind.css` must be generated
    // before the copy plugin runs.
    await dashboardBuilder.watch()
    const args = bundler.argumentsFromEnv({ supportsLocalBackend: true, supportsDeepLinks: false })
    const options = bundler.bundlerOptions(args)
    options.pure.splice(options.pure.indexOf('assert'), 1)
    options.define.REDIRECT_OVERRIDE = JSON.stringify('http://localhost:8080')
    // This is safe as this entry point is statically known.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const serviceWorkerEntryPoint = options.entryPoints.find(
        entryPoint => entryPoint.out === 'serviceWorker'
    )!
    serviceWorkerEntryPoint.in = path.resolve(THIS_PATH, 'src', 'devServiceWorker.ts')
    const builder = await esbuild.context(options)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: options.outdir,
        /** This function is called on every request.
         * It is used here to show an error if the file to serve was not found. */
        onRequest(request) {
            if (request.status !== HTTP_STATUS_OK) {
                console.error(
                    chalk.red(`HTTP error ${request.status} when serving path '${request.path}'.`)
                )
            }
        },
    })
}

void watch()
