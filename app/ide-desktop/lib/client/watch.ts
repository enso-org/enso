import * as clientBundler from './esbuild-config.js'
import esbuild from 'esbuild'
import {
    getGuiDirectory,
    getIdeDirectory,
    getProjectManagerBundle,
    getProjectManagerInBundlePath,
    project_manager_bundle
} from './paths.js'
import path from "node:path";
import fs from "node:fs/promises";
import * as assert from "assert";
import child_process from "node:child_process";
import * as contentBundler from '../content/esbuild-config.js'
import * as esbuildWatch from '../../esbuild-watch.js'
import {log} from "../../../../dist/wasm";


const guiDist = path.resolve(getGuiDirectory())
const ideDist = getIdeDirectory()
const projectManagerBundle = getProjectManagerBundle()

console.log("Cleaning IDE dist directory.")
await fs.rm(ideDist, {recursive: true, force: true})
await fs.mkdir(ideDist, {recursive: true})

// let contentBuilt = false
// let clientBuilt = false
//
// let electronProcess: child_process.ChildProcess = null;


console.log("Bundling client.")
const clientBundlerOpts: esbuild.BuildOptions = {
    ...clientBundler.bundlerOptionsFromEnv(),
    outdir: path.resolve(ideDist),
    watch: {
        onRebuild(error, result) {
            if (error) {
                console.error('Watch build failed:', error)
            } else {
                console.log('Client bundle updated.')
            }
        }
    }
}
await esbuild.build(clientBundlerOpts)


// const bundlerOptions = bundlerOptionsFromEnv()
// bundlerOptions.outdir = path.resolve(ideDist)
// await esbuild.build(bundlerOptions)

console.log("Bundling content.")
await esbuildWatch.watch({
    ...contentBundler.bundlerOptionsFromEnv(),
    outdir: path.resolve(ideDist, 'assets'),
}, () => {
    console.log("Content bundle updated.")
})

// await fs.cp(guiDist, path.join(ideDist), {recursive: true})

console.log("Exposing Project Manager bundle.")
await fs.symlink(projectManagerBundle,  path.join(ideDist, project_manager_bundle), 'dir')

console.log("Spawning Electron process.")
const electronArgs = [path.join(ideDist, 'index.cjs'), '--']
const process = child_process.spawn('electron', electronArgs, {stdio: 'inherit', shell: true})

// Wait till process finished.
await new Promise((resolve, reject) => {
    process.on('close', resolve)
    process.on('error', reject)
})
