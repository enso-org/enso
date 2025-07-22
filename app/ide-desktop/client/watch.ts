/**
 * @file This script is for watching the whole IDE and spawning the electron process.
 *
 * It sets up watchers for the client and content, and spawns the electron process with the IDE.
 * The spawned electron process can then use its refresh capability to pull the latest changes
 * from the watchers.
 */
import chalk from 'chalk'
import { spawn } from 'node:child_process'
import { mkdir, rm, symlink } from 'node:fs/promises'
import * as path from 'node:path'
import process from 'node:process'

import { BuildResult, context } from 'esbuild'

import { bundlerOptionsFromEnv } from './esbuildConfig'
import { getIdeDirectory, getProjectManagerBundlePath, PROJECT_MANAGER_BUNDLE } from './paths'

const IDE_DIR_PATH = getIdeDirectory()
const PROJECT_MANAGER_BUNDLE_PATH = getProjectManagerBundlePath()

// @ts-expect-error This is the only place where an environment variable should be written to.
process.env.ELECTRON_DEV_MODE = 'true'

console.log(chalk.cyan('Cleaning IDE dist directory.'))
await rm(IDE_DIR_PATH, { recursive: true, force: true })
await mkdir(IDE_DIR_PATH, { recursive: true })
const NODE_MODULES_PATH = path.resolve('./node_modules')

const BUNDLE_READY = new Promise<BuildResult>((resolve, reject) => {
  void (async () => {
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
            reject(result.errors[0])
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

    resolve(client)
  })()
})

await BUNDLE_READY
console.log(
  chalk.cyan(
    `Linking Project Manager bundle at '${PROJECT_MANAGER_BUNDLE_PATH}' to '${path.join(
      IDE_DIR_PATH,
      PROJECT_MANAGER_BUNDLE,
    )}'.`,
  ),
)
await symlink(PROJECT_MANAGER_BUNDLE_PATH, path.join(IDE_DIR_PATH, PROJECT_MANAGER_BUNDLE), 'dir')

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

/** Starts the electron process with the IDE. */
function startElectronProcess() {
  console.log(chalk.cyan('Spawning Electron process.'))

  const electronProcess = spawn('electron', ELECTRON_ARGS, {
    stdio: 'inherit',
    shell: true,
    env: Object.assign({ NODE_MODULES_PATH }, process.env),
  })

  electronProcess.on('close', (code) => {
    if (code === 0) {
      electronProcess.removeAllListeners()
      exit()
    }
  })

  electronProcess.on('error', (error) => {
    console.error(chalk.red('Electron process failed:'), error)
    console.error(chalk.red('Killing electron process.'))
    electronProcess.removeAllListeners()
    electronProcess.kill()
    exit(1)
  })
}

startElectronProcess()
