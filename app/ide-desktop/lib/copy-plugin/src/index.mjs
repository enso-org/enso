/** @file This plugin copies files to the esbuild's output directory while registering them for watching.
 *
 * This plugin registers faux entry point and then intercepts it during resolution and then copies
 * the files and registers them for watching.
 *
 * This is a workaround-ish solution. Unfortunately, only resolve/load plugin APIs allow adding new
 * watch files. */

import * as fs from 'node:fs'
import * as path from 'node:path'

// =================
// === Constants ===
// =================

const PLUGIN_NAME = 'enso-copy-plugin'
const COPY_OPTIONS = {
    recursive: true,
    force: true,
    dereference: true,
}

// ===============
// === Helpers ===
// ===============

/**
 * File copy with logging.
 *
 * @param {string} from - Path to the file to copy.
 * @param {string} to - Path of the new file.
 */
async function copy(from, to) {
    console.log(`Copying ${from} to ${to}`)
    await fs.promises.cp(from, to, COPY_OPTIONS)
}

// ==============
// === Plugin ===
// ==============

/**
 * Create a plugin instance.
 *
 * @param {() => AsyncGenerator<string>} filesProvider - Invocable that yields
 * an async-iterable object listing files to copy.
 */
export function create(filesProvider) {
    /** Sets up the esbuild plugin.
     * @param {import('esbuild').PluginBuild} build - Esbuild build options.
     * @throws {Error} When `build.entryPoints` is neither an array nor an object.
     */
    let setup = build => {
        console.log(`Instantiating copy plugin for the build.`)
        let magic = 'COPY_ASSETS_MARKER'
        /** @type {AsyncGenerator<string>} */
        let files

        if (Array.isArray(build.initialOptions.entryPoints)) {
            build.initialOptions.entryPoints.push(magic)
        } else if (typeof build.initialOptions.entryPoints === 'object') {
            build.initialOptions.entryPoints[magic] = magic
        } else {
            throw new Error(
                `Invalid entryPoints: ${JSON.stringify(build.initialOptions.entryPoints)}`
            )
        }

        build.onStart(() => {
            console.log('Initial options:', build.initialOptions)
            console.log('Collecting files to copy.')
            files = filesProvider()
        })
        build.onResolve({ filter: new RegExp(magic) }, resolve => {
            console.log('Resolving ', resolve)
            return {
                path: magic,
                namespace: PLUGIN_NAME,
            }
        })
        build.onLoad({ filter: /.*/, namespace: PLUGIN_NAME }, async () => {
            if (build.initialOptions.outdir == null) {
                console.error('`copy-plugin` requires `outdir` to be specified.')
                return
            } else {
                let watchFiles = []
                for await (const file of files) {
                    const to = path.join(build.initialOptions.outdir, path.basename(file))
                    await copy(file, to)
                    watchFiles.push(file)
                }
                console.log('Copied files.', watchFiles)
                return {
                    contents: '',
                    watchFiles,
                }
            }
        })
        build.onEnd(() => {
            // Replace with empty `AsyncGenerator`.
            files = (async function* () {
                yield* await Promise.resolve([])
            })()
        })
    }
    return { name: PLUGIN_NAME, setup }
}

export default { create }
