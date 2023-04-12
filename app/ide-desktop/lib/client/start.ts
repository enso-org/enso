/** @file This script starts the IDE using the Electron executable. */

import * as childProcess from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'

import * as esbuild from 'esbuild'

import * as esbuildConfig from './esbuild-config'
import * as paths from './paths'

const GUI_PATH = path.resolve(paths.getGuiDirectory())
const IDE_PATH = paths.getIdeDirectory()
const PROJECT_MANAGER_BUNDLE = paths.getProjectManagerBundlePath()

const SCRIPT_ARGS = process.argv.slice(2)
console.log('Script arguments:', ...SCRIPT_ARGS.map(arg => JSON.stringify(arg)))

console.log('Cleaning IDE dist directory.')
await fs.rm(IDE_PATH, { recursive: true, force: true })
await fs.mkdir(IDE_PATH, { recursive: true })

console.log('Bundling client.')
const BUNDLER_OPTIONS = esbuildConfig.bundlerOptionsFromEnv()
BUNDLER_OPTIONS.outdir = path.resolve(IDE_PATH)
await esbuild.build(BUNDLER_OPTIONS)

console.log('Linking GUI files.')
await fs.symlink(path.join(GUI_PATH, 'assets'), path.join(IDE_PATH, 'assets'), 'dir')

console.log('LinkingProject Manager files.')
await fs.symlink(PROJECT_MANAGER_BUNDLE, path.join(IDE_PATH, paths.PROJECT_MANAGER_BUNDLE), 'dir')

console.log('Spawning Electron process.')
const ELECTRON_ARGS = [path.join(IDE_PATH, 'index.cjs'), '--', ...SCRIPT_ARGS]
const ELECTRON_PROCESS = childProcess.spawn('electron', ELECTRON_ARGS, {
    stdio: 'inherit',
    shell: true,
})

// Wait till process finished.
const CODE = await new Promise<string>((resolve, reject) => {
    ELECTRON_PROCESS.on('close', resolve)
    ELECTRON_PROCESS.on('error', reject)
})
console.log(`Electron process finished. Exit code: ${CODE}.`)
