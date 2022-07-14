import path from 'node:path'
import fs from 'node:fs'

export function create(files_provider) {
    let name = 'enso-copy-plugin'
    let setup = build => {
        console.log(`Instantiating copy plugin for the build.`, build)
        let magic = 'COPY_ASSETS_MARKER'
        let files = []
        let to = path.resolve(build.initialOptions.outdir)

        const copy = async (from, to) => {
            const cpOpts = {
                recursive: true,
                force: true,
                dereference: true,
            }
            console.log(`Copying ${from} to ${to}`)
            await fs.promises.cp(from, to, cpOpts)
        }
        if (typeof build.initialOptions.entryPoints === 'array') {
            build.initialOptions.entryPoints.push(magic)
        } else if (typeof build.initialOptions.entryPoints === 'object') {
            build.initialOptions.entryPoints[magic] = magic
        }
        build.onStart(async () => {
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
