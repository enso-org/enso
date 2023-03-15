/** This plugin copies files to the esbuild's output directory while registering them for watching.
 *
 * This plugin registers faux entry point and then intercepts it during resolution and then copies
 * the files and registers them for watching.
 *
 * This is a workaround-ish solution. Unfortunately, only resolve/load plugin APIs allow adding new
 * watch files.
 *
 */

import path from 'node:path'
import fs from 'node:fs'

const copyOptions = {
    recursive: true,
    force: true,
    dereference: true,
}

async function copy(from, to) {
    console.log(`Copying ${from} to ${to}`)
    await fs.promises.cp(from, to, copyOptions)
}

/**
 * Create a plugin instance.
 *
 * @param files_provider Invocable that yields an async-iterable object listing files to copy.
 */
export function create(files_provider) {
    let name = 'enso-copy-plugin'
    let setup = build => {
        console.log(`Instantiating copy plugin for the build.`)
        let magic = 'COPY_ASSETS_MARKER'
        let files = []

        if (Array.isArray(build.initialOptions.entryPoints)) {
            build.initialOptions.entryPoints.push(magic)
        } else if (typeof build.initialOptions.entryPoints === 'object') {
            build.initialOptions.entryPoints[magic] = magic
        } else {
            throw new Error(`Invalid entryPoints: ${build.initialOptions.entryPoints}`)
        }

        build.onStart(async () => {
            console.log('Initial options:', build.initialOptions)
            console.log('Collecting files to copy.')
            files = files_provider()
        })
        build.onResolve({ filter: new RegExp(magic) }, async resolve => {
            console.log('Resolving ', resolve)
            return {
                path: magic,
                namespace: name,
            }
        })
        build.onLoad({ filter: /.*/, namespace: name }, async arg => {
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
        })
        build.onEnd(() => {
            files = []
        })
    }
    return { name, setup }
}

export default { create }
