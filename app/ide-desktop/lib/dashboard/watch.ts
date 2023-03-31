/** @file File watch and compile service. */
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import chalk from 'chalk'

import * as bundler from './esbuild-config'

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

// =================
// === Constants ===
// =================

/** This must be port `8081` because it is defined as such in AWS. */
const PORT = 8081
const HTTP_STATUS_OK = 200
// `outputPath` does not have to be a real directory because `write` is `false`,
// meaning that files will not be written to the filesystem.
// However, the path should still be non-empty in order for `esbuild.serve` to work properly.
// The path must also not be `/` otherwise SSEs are not sent on rebuilds.
const ARGS: bundler.Arguments = { outputPath: '/tmp/', devMode: true }
const OPTS = bundler.bundlerOptions(ARGS)
OPTS.entryPoints.push(
    path.resolve(THIS_PATH, 'src', 'index.html'),
    path.resolve(THIS_PATH, 'src', 'index.tsx'),
    path.resolve(THIS_PATH, 'src', 'serviceWorker.ts')
)
OPTS.minify = false
OPTS.write = false

// ===============
// === Watcher ===
// ===============

async function watch() {
    const builder = await esbuild.context(OPTS)
    await builder.watch()
    await builder.serve({
        port: PORT,
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
