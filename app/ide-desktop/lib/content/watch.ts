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

const PORT = 8080
const HTTP_STATUS_OK = 200
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// ===============
// === Watcher ===
// ===============

async function watch() {
    const dashboardOpts = dashboardBundler.bundleOptions()
    const dashboardBuilder = await esbuild.context(dashboardOpts)
    // We do not need to serve the dashboard as it outputs to the same directory.
    // It will not rebuild on request, but it is not intended to rebuild on request anyway.
    // This MUST be called before `builder.watch()` as `tailwind.css` must be generated
    // before the copy plugin runs.
    await dashboardBuilder.watch()
    const opts = bundler.bundlerOptions({
        ...bundler.argumentsFromEnv(),
        devMode: true,
    })
    opts.entryPoints.push({
        in: path.resolve(THIS_PATH, 'src', 'serviceWorker.ts'),
        out: 'serviceWorker',
    })
    const builder = await esbuild.context(opts)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: opts.outdir,
        onRequest(args) {
            if (args.status !== HTTP_STATUS_OK) {
                console.error(
                    chalk.red(`HTTP error ${args.status} when serving path '${args.path}'.`)
                )
            }
        },
    })
}

void watch()
