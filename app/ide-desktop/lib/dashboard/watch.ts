/** @file File watch and compile service. */
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import * as portfinder from 'portfinder'
import chalk from 'chalk'

import * as bundler from './esbuild-config'

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =================
// === Constants ===
// =================

const PORT = 8081
const HTTP_STATUS_OK = 200
const OPTS = bundler.defaultBundlerOptions({ devMode: true })

// ===============
// === Watcher ===
// ===============

async function watch() {
    OPTS.entryPoints.push(path.resolve(THIS_PATH, 'src', 'index.html'))
    const builder = await esbuild.context(OPTS)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: OPTS.outdir,
        onRequest(args) {
            if (args.status !== HTTP_STATUS_OK) {
                console.error(chalk.red(`HTTP error ${args.status} when serving path '${args.path}'.`))
            }
        },
    })
}

void watch()
