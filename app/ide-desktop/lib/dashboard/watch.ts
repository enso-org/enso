/** @file File watch and compile service. */
import * as childProcess from 'node:child_process'
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
const TAILWIND_BINARY_PATH = '../../node_modules/.bin/tailwindcss'

// ===============
// === Watcher ===
// ===============

async function watch() {
    const opts = bundler.defaultBundlerOptions({ devMode: true })
    const dest = path.join(opts.outdir, 'tailwind.css')
    const config = path.resolve(THIS_PATH, 'tailwind.config.ts')
    const tailwindCssPath = path.resolve(THIS_PATH, 'src', 'tailwind.css')
    console.log(
        `Generating tailwind css from '${tailwindCssPath}' to '${dest}'.`
    )
    childProcess.spawnSync(`node`, [
        TAILWIND_BINARY_PATH,
        '-i',
        tailwindCssPath,
        '-o',
        dest,
        '-c',
        config,
        '--minify',
    ])
    opts.entryPoints.push(path.resolve(THIS_PATH, 'src', 'index.html'))
    const builder = await esbuild.context(opts)
    await builder.watch()
    await builder.serve({
        port: await portfinder.getPortPromise({ port: PORT }),
        servedir: opts.outdir,
        onRequest(args) {
            if (args.status !== HTTP_STATUS_OK) {
                console.error(chalk.red(`HTTP error ${args.status} when serving path '${args.path}'.`))
            }
        },
    })
}

void watch()
