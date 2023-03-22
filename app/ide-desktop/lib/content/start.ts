/** @file Start the file watch service. */
import * as esbuild from 'esbuild'
import * as portfinder from 'portfinder'

import * as bundler from './esbuild-config.js'

const PORT = 8080
const HTTP_STATUS_OK = 200

async function watch() {
    const opts = bundler.bundleOptions()
    const builder = await esbuild.context(opts)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: opts.outdir,
        onRequest(args) {
            if (args.status !== HTTP_STATUS_OK) {
                console.error(`HTTP error ${args.status} when serving path '${args.path}'.`)
            }
        },
    })
}

void watch()
