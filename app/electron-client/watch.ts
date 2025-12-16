/**
 * @file This script is for watching the whole IDE and spawning the electron process.
 *
 * It sets up watchers for the client and content, and spawns the electron process with the IDE.
 * The spawned electron process can then use its refresh capability to pull the latest changes
 * from the watchers.
 */
import chalk from 'chalk'
import { context, type BuildResult } from 'esbuild'
import { spawn } from 'node:child_process'
import { mkdir, rm, symlink } from 'node:fs/promises'
import * as path from 'node:path'
import process from 'node:process'
import { bundlerOptionsFromEnv } from './esbuildConfig'
import { getBackendBundlePath, getIdeDirectory } from './paths'

const IDE_DIR_PATH = getIdeDirectory()
const BACKEND_BUNDLE_PATH = getBackendBundlePath()

// @ts-expect-error This is the only place where an environment variable should be written to.
process.env.ELECTRON_DEV_MODE = 'true'

console.log(chalk.cyan('Cleaning IDE dist directory.'))
await rm(IDE_DIR_PATH, { recursive: true, force: true })
await mkdir(IDE_DIR_PATH, { recursive: true })
const NODE_MODULES_PATH = path.resolve('./node_modules')
const GUI_CONFIG_PATH = path.resolve('../gui/vite.config.ts')

const BUNDLE_READY = (async (): Promise<BuildResult> => {
  console.log(chalk.cyan('Bundling client.'))
  const devMode = true
  const clientBundlerOpts = bundlerOptionsFromEnv(devMode)
  clientBundlerOpts.outdir = path.resolve(IDE_DIR_PATH)
  ;(clientBundlerOpts.plugins ??= []).push({
    name: 'enso-on-rebuild',
    setup: (build) => {
      build.onEnd((result) => {
        if (result.errors.length) {
          // We cannot carry on if the client failed to build, because electron
          // would immediately exit with an error.
          console.error(chalk.red('Client bundle update failed:'), result.errors[0])
          throw result.errors[0]
        } else {
          console.log(chalk.green('Client bundle updated.'))
          for (const error of result.errors) {
            console.error(error)
          }
          for (const warning of result.warnings) {
            console.warn(warning)
          }
        }
      })
    },
  })
  const clientBuilder = await context(clientBundlerOpts)
  const client = await clientBuilder.rebuild()
  void clientBuilder.watch()
  return client
})()

await BUNDLE_READY
console.log(
  chalk.cyan(
    `Linking Backend bundle at '${BACKEND_BUNDLE_PATH}' to '${path.join(IDE_DIR_PATH, 'enso')}'.`,
  ),
)
await symlink(BACKEND_BUNDLE_PATH, path.join(IDE_DIR_PATH, 'enso'), 'dir')

const ELECTRON_FLAGS =
  process.env.ELECTRON_FLAGS == null ? [] : String(process.env.ELECTRON_FLAGS).split(' ')
const ELECTRON_ARGS = [
  path.join(IDE_DIR_PATH, 'index.mjs'),
  ...ELECTRON_FLAGS,
  '--',
  ...process.argv.slice(2).map((arg) => `'${arg}'`),
]

const exit = (code = 0) => {
  void rm(IDE_DIR_PATH, { recursive: true, force: true }).then(() => {
    // The `esbuild` process seems to remain alive at this point and will keep our process
    // from ending. Thus, we exit manually. It seems to terminate the child `esbuild` process
    // as well.
    process.exit(code)
  })
}

process.on('SIGINT', () => {
  exit()
})

// Start the electron process with the IDE.

console.log(chalk.cyan('Spawning Electron process.'))

const electronProcess = spawn('electron', ELECTRON_ARGS, {
  stdio: 'inherit',
  shell: true,
  env: Object.assign({ NODE_MODULES_PATH, GUI_CONFIG_PATH }, process.env),
})
  .on('close', (code) => {
    if (code === 0) {
      electronProcess.removeAllListeners()
      exit()
    }
  })
  .on('error', (error) => {
    console.error(chalk.red('Electron process failed:'), error)
    console.error(chalk.red('Killing electron process.'))
    electronProcess.removeAllListeners()
    electronProcess.kill()
    exit(1)
  })
  .on('exit', (code) => {
    console.log((code ? chalk.red : chalk.cyan)(`Electron process exited with code ${code}.`))
    exit(code ?? 0)
  })
