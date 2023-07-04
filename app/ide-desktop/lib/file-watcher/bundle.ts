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

// ===============
// === Bundler ===
// ===============

/** Clean up old build output and runs the esbuild bundler. */
async function bundle() {
    try {
        try {
            await fs.rm('./dist', { recursive: true })
        } catch {
            // Ignored.
        }
        const opts = bundler.bundlerOptions({
            outputPath: './dist',
            devMode: false,
        })
        await esbuild.build(opts)
        return
    } catch (error) {
        console.error(error)
        // The error is being re-thrown.
        // eslint-disable-next-line no-restricted-syntax
        throw error
    }
}

void bundle()
