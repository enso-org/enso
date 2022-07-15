import path from 'node:path'
import fs from 'node:fs'

export function create(files_provider) {
    let name = 'enso-copy-plugin'
    let setup = build => {
        console.log(`Instantiating copy plugin for the build.`)
        let magic = 'COPY_ASSETS_MARKER'
        let files = []

        const copy = async (from, to) => {
            const cpOpts = {
                recursive: true,
                force: true,
                dereference: true,
            }
            console.log(`Copying ${from} to ${to}`)
            await fs.promises.cp(from, to, cpOpts)
        }
        console.log(build.initialOptions.entryPoints)
        console.log(Object.prototype.toString.call(build.initialOptions.entryPoints))

        if (Array.isArray(build.initialOptions.entryPoints)) {
            build.initialOptions.entryPoints.push(magic)
        } else if (typeof build.initialOptions.entryPoints === 'object') {
            build.initialOptions.entryPoints[magic] = magic
        } else {
            console.error(`Invalid entryPoints:`, build.initialOptions.entryPoints)
            throw new Error(`Invalid entryPoints: ${build.initialOptions.entryPoints}`)
        }

        build.onStart(async () => {
            console.log('Initial options:', build.initialOptions)
            files = files_provider()
            console.log('Collecting files to copy.', files)
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
