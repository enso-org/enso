/** @file Start the file watch service. */
import * as esbuild from 'esbuild'
import * as portfinder from 'portfinder'

import * as bundler from './esbuild-config.js'

// =================
// === Constants ===
// =================

const PORT = 8080
const HTTP_STATUS_OK = 200

// ===============
// === Watcher ===
// ===============

/** Start the esbuild watcher. */
async function watch() {
    const opts = bundler.bundleOptions()
    const builder = await esbuild.context(opts)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: opts.outdir,
        /** This function is called on every request.
         * It is used here to show an error if the file to serve was not found. */
        onRequest(args) {
            if (args.status !== HTTP_STATUS_OK) {
                console.error(`HTTP error ${args.status} when serving path '${args.path}'.`)
            }
        },
    })
}

void watch()
