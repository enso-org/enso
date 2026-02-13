import { assetsPath } from '@/paths'
import { extractFile } from '@electron/asar'
import type { $Config } from 'enso-gui/src/config'
import { createRequire } from 'node:module'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { runInNewContext } from 'node:vm'

const GUI_ENV_PREFIX = 'ENSO_IDE_'
const GUI_ENV_DIR = fileURLToPath(new URL('../../gui', import.meta.url))

type GuiEnv = Record<string, string | undefined>

const GUI_CONFIG_KEYS = [
  'ENVIRONMENT',
  'HOST',
  'API_URL',
  'SENTRY_DSN',
  'STRIPE_KEY',
  'AUTH_ENDPOINT',
  'COGNITO_USER_POOL_ID',
  'COGNITO_USER_POOL_WEB_CLIENT_ID',
  'GOOGLE_ANALYTICS_TAG',
  'COGNITO_DOMAIN',
  'COGNITO_REGION',
  'VERSION',
  'COMMIT_HASH',
  'YDOC_SERVER_URL',
  'CLOUD_BUILD',
  'AG_GRID_LICENSE_KEY',
  'GOOGLE_OAUTH_CLIENT_ID',
  'STRAVA_OAUTH_CLIENT_ID',
  'MS365_OAUTH_CLIENT_ID',
  'SALESFORCE_OAUTH_CLIENT_ID',
] as const satisfies readonly (keyof $Config)[]

type _AssertAllGuiConfigKeysCovered =
  Exclude<keyof $Config, (typeof GUI_CONFIG_KEYS)[number]> extends never ? true : never
const _assertAllGuiConfigKeysCovered: _AssertAllGuiConfigKeysCovered = true

type GuiConfigKey = (typeof GUI_CONFIG_KEYS)[number]

function envVarNameForKey(key: GuiConfigKey): string {
  return `${GUI_ENV_PREFIX}${key}`
}

function buildGuiConfig(getValue: (key: GuiConfigKey) => string | undefined): $Config {
  return Object.fromEntries(GUI_CONFIG_KEYS.map((key) => [key, getValue(key)])) as $Config
}

/** ENSO_IDE_<name> environment variable is loaded as <name>. */
function configFromEnv(env: GuiEnv): $Config {
  return buildGuiConfig((key) => env[envVarNameForKey(key)])
}

/** Same as {@link configFromEnv}, but values from `baseConfig` are used as a fallback. */
function configFromBaseAndEnv(baseConfig: $Config, env: GuiEnv): $Config {
  return buildGuiConfig((key) => env[envVarNameForKey(key)] ?? baseConfig[key])
}

let cachedGuiConfigPromise: Promise<$Config> | undefined

async function loadGuiConfigUncached(): Promise<$Config> {
  if (process.env.ELECTRON_DEV_MODE === 'true') {
    // This codepath is only used in Electron watch mode, to load the GUI config values from .env files in the repository.
    const nodeModulesPath = process.env.NODE_MODULES_PATH
    const require = createRequire(`${nodeModulesPath}/package.json`)
    const vitePath = require.resolve('vite')
    const { loadEnv } = await import(vitePath)
    // Keep consistent with the Electron content server Vite middleware.
    const mode = process.env.MODE ?? 'staging'
    const envFromFiles = loadEnv(mode, GUI_ENV_DIR, GUI_ENV_PREFIX)
    const fullEnv = { ...envFromFiles, ...process.env } as GuiEnv
    return configFromEnv(fullEnv)
  }

  const assetsDir = assetsPath(undefined)
  if (assetsDir.includes('.asar')) {
    const archivePath = assetsDir.slice(0, assetsDir.indexOf('.asar') + '.asar'.length)
    // TODO[ib]: This ridiculous duplication of `assets` in path is a bug in legacy build system, Bazel build has a different path.
    const configPath = path.join('assets', 'assets', 'config.js')
    const baseConfig = await importConfigFromAsar(archivePath, configPath)
    const env = process.env as unknown as GuiEnv
    return configFromBaseAndEnv(baseConfig, env)
  } else {
    throw new Error('Assets are expected to be in ASAR archive, but it is not found.')
  }
}

async function importConfigFromAsar(archivePath: string, innerPath: string): Promise<$Config> {
  const contents = extractFile(archivePath, innerPath)
  const sandbox = { window: {} as Record<string, unknown>, process }
  runInNewContext(contents.toString('utf8'), sandbox)
  return sandbox.window.$config as $Config
}

/** Extract and load GUI config from ASAR archive in Electron distribution, or from .env files in Electron dev mode. */
export async function loadGuiConfig(): Promise<$Config> {
  cachedGuiConfigPromise ??= loadGuiConfigUncached()
  return await cachedGuiConfigPromise
}
