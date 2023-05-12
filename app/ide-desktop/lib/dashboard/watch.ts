/** @file File watch and compile service. */
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'
import chalk from 'chalk'

import * as bundler from './esbuild-config'

// =================
// === Constants ===
// =================

/** The path of this file. */
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
/** This must be port `8080` because it is defined as such in AWS. */
const PORT = 8080
const HTTP_STATUS_OK = 200
// `outputPath` does not have to be a real directory because `write` is `false`,
// meaning that files will not be written to the filesystem.
// However, the path should still be non-empty in order for `esbuild.serve` to work properly.
const ARGS: bundler.Arguments = { outputPath: '/', devMode: true }
const OPTS = bundler.bundlerOptions(ARGS)
OPTS.entryPoints.push(
    path.resolve(THIS_PATH, 'src', 'index.html'),
    path.resolve(THIS_PATH, 'src', 'index.tsx'),
    path.resolve(THIS_PATH, 'src', 'serviceWorker.ts')
)
OPTS.write = false
// eslint-disable-next-line @typescript-eslint/naming-convention
OPTS.loader = { '.html': 'copy' }

// ===============
// === Watcher ===
// ===============

/** Start the esbuild watcher. */
async function watch() {
    const builder = await esbuild.context(OPTS)
    await builder.watch()
    await builder.serve({
        port: PORT,
        servedir: OPTS.outdir,
        /** This function is called on every request.
         * It is used here to show an error if the file to serve was not found. */
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
