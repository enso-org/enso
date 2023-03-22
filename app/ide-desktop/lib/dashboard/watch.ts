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
// `assetsPath` and `outputPath` do not have to be real directories because `write` is `false`,
// meaning that files will not be written to the filesystem.
// However, they should still have the correct paths relative to each other
// in order for `esbuild.serve` to work properly.
const ARGS: bundler.Arguments = { assetsPath: '/', outputPath: '/', devMode: true }
const OPTS = bundler.bundlerOptions(ARGS)
OPTS.entryPoints.push(
    path.resolve(THIS_PATH, 'src', 'index.html'),
    path.resolve(THIS_PATH, 'src', 'index.tsx'),
    path.resolve(THIS_PATH, 'src', 'serviceWorker.ts')
)
OPTS.write = false

// ===============
// === Watcher ===
// ===============

async function watch() {
    const builder = await esbuild.context(OPTS)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: OPTS.outdir,
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
