/** @file Entry point for the bundler. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

// =================
// === Constants ===
// =================

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))
export const ANALYZE = process.argv.includes('--analyze')

// ===============
// === Bundler ===
// ===============

/** Clean up old build output and runs the esbuild bundler. */
async function bundle() {
    try {
        try {
            await fs.rm('./build', { recursive: true })
        } catch {
            // Ignored.
        }
        const opts = bundler.bundlerOptions({
            outputPath: './build',
            devMode: false,
        })
        opts.entryPoints.push(
            path.resolve(THIS_PATH, 'src', 'index.html'),
            path.resolve(THIS_PATH, 'src', 'index.ts')
        )
        opts.metafile = ANALYZE
        opts.loader['.html'] = 'copy'
        const result = await esbuild.build(opts)
        if (result.metafile) {
            console.log(await esbuild.analyzeMetafile(result.metafile))
        }
        return
    } catch (error) {
        console.error(error)
        // The error is being re-thrown.
        // eslint-disable-next-line no-restricted-syntax
        throw error
    }
}

void bundle()
