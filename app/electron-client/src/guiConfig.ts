import { assetsPath } from '@/paths'
import { extractFile } from '@electron/asar'
import type { $Config } from 'enso-gui/src/config'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'
import { runInNewContext } from 'node:vm'

const GUI_ENV_PREFIX = 'ENSO_IDE_'
const GUI_ENV_DIR = fileURLToPath(new URL('../../gui', import.meta.url))

function envToConfig(env: Record<string, string | undefined>): $Config {
  // Conversion is technically unsafe, but practically is only used in Electron watch mode.
  return Object.fromEntries(
    Object.entries(env).filter(([key]) => key.startsWith(GUI_ENV_PREFIX)),
  ) as $Config
}

async function importConfigFromAsar(archivePath: string, innerPath: string): Promise<$Config> {
  const contents = extractFile(archivePath, innerPath)
  const sandbox = { window: {} as Record<string, unknown>, process }
  runInNewContext(contents.toString('utf8'), sandbox)
  return sandbox.window.$config as $Config
}

/** Extract and load GUI config from ASAR archive in Electron distribution, or from .env files in Electron dev mode. */
export async function loadGuiConfig(): Promise<$Config> {
  if (process.env.ELECTRON_DEV_MODE === 'true') {
    // This codepath is only used in Electron watch mode, to load the GUI config values from .env files in the repository.
    const nodeModulesPath = process.env.NODE_MODULES_PATH
    const require = createRequire(`${nodeModulesPath}/package.json`)
    const vitePath = require.resolve('vite')
    const { loadEnv } = await import(vitePath)
    const mode = process.env.MODE ?? process.env.NODE_ENV ?? 'development'
    const env = loadEnv(mode, GUI_ENV_DIR, GUI_ENV_PREFIX)
    return envToConfig({ ...process.env, ...env })
  }

  const assetsDir = assetsPath(undefined)
  if (assetsDir.includes('.asar')) {
    const archivePath = assetsDir.slice(0, assetsDir.indexOf('.asar') + '.asar'.length)
    // TODO[ib]: This ridiculous duplication of `assets` in path is a bug in legacy build system, Bazel build has a different path.
    return await importConfigFromAsar(archivePath, 'assets/assets/config.js')
  } else {
    throw new Error('Assets are expected to be in ASAR archive, but it is not found.')
  }
}
