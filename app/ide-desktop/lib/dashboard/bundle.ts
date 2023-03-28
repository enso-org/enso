/** @file Entry point for the bundler. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

import * as esbuild from 'esbuild'

import * as bundler from './esbuild-config'

export const THIS_PATH = path.resolve(path.dirname(url.fileURLToPath(import.meta.url)))

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
            path.resolve(THIS_PATH, 'src', 'index.tsx')
        )
        await esbuild.build(opts)
    } catch (error) {
        console.error(error)
        throw error
    }
}

void bundle()
