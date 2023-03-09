/** This script starts the IDE using the Electron executable. */

import { bundlerOptionsFromEnv, outdir } from './esbuild-config.js'
import esbuild from 'esbuild'
import {
    getGuiDirectory,
    getIdeDirectory,
    getProjectManagerBundle,
    getProjectManagerInBundlePath,
    project_manager_bundle,
} from './paths.js'
import path from 'node:path'
import fs from 'node:fs/promises'
import * as assert from 'assert'
import child_process from 'node:child_process'

const guiDist = path.resolve(getGuiDirectory())
const ideDist = getIdeDirectory()
const projectManagerBundle = getProjectManagerBundle()

const scriptArgs = process.argv.slice(2)
console.log('Script arguments:')

console.log('Cleaning IDE dist directory.')
await fs.rm(ideDist, { recursive: true, force: true })
await fs.mkdir(ideDist, { recursive: true })

console.log('Bundling client.')
const bundlerOptions = bundlerOptionsFromEnv()
bundlerOptions.outdir = path.resolve(ideDist)
await esbuild.build(bundlerOptions)

console.log('Linking GUI files.')
await fs.symlink(path.join(guiDist, 'assets'), path.join(ideDist, 'assets'), 'dir')

console.log('LinkingProject Manager files.')
await fs.symlink(projectManagerBundle, path.join(ideDist, project_manager_bundle), 'dir')

console.log('Spawning Electron process.')
const electronArgs = [path.join(ideDist, 'index.cjs'), '--', ...scriptArgs]
const electronProcess = child_process.spawn('electron', electronArgs, {
    stdio: 'inherit',
    shell: true,
})

// Wait till process finished.
const code = await new Promise((resolve, reject) => {
    electronProcess.on('close', resolve)
    electronProcess.on('error', reject)
})
console.log(`Electron process finished. Exit code: ${code}.`)
