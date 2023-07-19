/** @file File watch and compile service. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// =================
// === Constants ===
// =================

/** The path of this file. */
const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
/** This must be port `8080` because it is defined as such in AWS. */
const PORT = 8080
// `outputPath` does not have to be a real directory because `write` is `false`,
// meaning that files will not be written to the filesystem.
// However, the path should still be non-empty in order for `esbuild.serve` to work properly.
const OPTS = bundler.bundlerOptions({ outputPath: '/', devMode: true })
OPTS.define.REDIRECT_OVERRIDE = JSON.stringify(`http://localhost:${PORT}`)
OPTS.entryPoints.push(
    path.resolve(THIS_PATH, 'src', 'index.html'),
    path.resolve(THIS_PATH, 'src', 'index.tsx'),
    path.resolve(THIS_PATH, 'src', 'serviceWorker.ts')
)
OPTS.write = false
OPTS.loader['.html'] = 'copy'
OPTS.plugins.push({
    name: 'inject-mock-modules',
    setup: build => {
        build.onResolve({ filter: /(?:)/ }, async args => {
            if (/^\.\.?\//.test(args.path)) {
                const mockPath = path
                    .resolve(args.importer, args.path)
                    .replace('/lib/dashboard/src/', '/lib/dashboard/mock/')
                try {
                    await fs.access(mockPath, fs.constants.R_OK)
                    return { path: mockPath }
                } catch {
                    return
                }
            }
        })
    },
})

// ===============
// === Watcher ===
// ===============

/** Start the esbuild watcher. */
async function serve() {
    const builder = await esbuild.context(OPTS)
    await builder.serve({
        port: PORT,
        servedir: OPTS.outdir,
    })
}

void serve()
