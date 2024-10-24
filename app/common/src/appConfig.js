/** @file Functions for managing app configuration. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as process from 'node:process'
import * as url from 'node:url'

// ===============================
// === readEnvironmentFromFile ===
// ===============================

/** Read environment variables from a file based on the `ENSO_CLOUD_ENV_FILE_NAME`
 * environment variable. Reads from `.env` if the variable is `production`, blank or absent.
 * DOES NOT override existing environment variables if the variable is absent. */
export async function readEnvironmentFromFile() {
  const environment = process.env.ENSO_CLOUD_ENVIRONMENT ?? null
  const isProduction = environment == null || environment === '' || environment === 'production'
  const fileName = isProduction ? '.env' : `.${environment}.env`
  const filePath = path.join(url.fileURLToPath(new URL('../..', import.meta.url)), fileName)
  const buildInfo = await (async () => {
    try {
      return await import('../../../build.json', { with: { type: 'json' } })
    } catch {
      return { commit: '', version: '', engineVersion: '', name: '' }
    }
  })()
  try {
    const file = await fs.readFile(filePath, { encoding: 'utf-8' })
    /** @type {readonly (readonly [string, string])[]} */
    let entries = file.split('\n').flatMap(line => {
      if (/^\s*$|^.s*#/.test(line)) {
        return []
      } else {
        const [key = '', value = ''] = line.split('=', 2)
        return [[key, value]]
      }
    })
    if (isProduction) {
      entries = entries.filter(kv => {
        const [k] = kv
        return process.env[k] == null
      })
    }
    const variables = Object.fromEntries(entries)
    if (!isProduction || entries.length > 0) {
      Object.assign(process.env, variables)
    }
    process.env.ENSO_CLOUD_DASHBOARD_VERSION ??= buildInfo.version
    process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH ??= buildInfo.commit
  } catch (error) {
    process.env.ENSO_CLOUD_DASHBOARD_VERSION ??= buildInfo.version
    process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH ??= buildInfo.commit
    const expectedKeys = Object.keys(DUMMY_DEFINES)
      .map(key => key.replace(/^process[.]env[.]/, ''))
      .filter(key => key !== 'NODE_ENV')
    /** @type {string[]} */
    const missingKeys = []
    for (const key of expectedKeys) {
      if (!(key in process.env)) {
        missingKeys.push(key)
      }
    }
    if (missingKeys.length !== 0) {
      console.warn('Could not load `.env` file; disabling cloud backend.')
      console.warn(`Missing keys: ${missingKeys.map(key => `'${key}'`).join(', ')}`)
      console.error(error)
    }
  }
}

// ===============
// === globals ===
// ===============

/** The value as JSON if it is not nullish, else `'undefined'`.
 * @param {unknown} value - the value to `JSON.stringify()`. */
function stringify(value) {
  return value == null ? 'undefined' : JSON.stringify(value)
}

/** Return an object containing app configuration to inject.
 *
 * This includes:
 * - the base URL for backend endpoints
 * - the WebSocket URL for the chatbot
 * - the unique identifier for the cloud environment, for use in Sentry logs
 * - Stripe, Sentry and Amplify public keys */
export function getDefines() {
  return {
    'process.env.ENSO_CLOUD_ENVIRONMENT': stringify(
      // The actual environment variable does not necessarily exist.
      process.env.ENSO_CLOUD_ENVIRONMENT ?? 'production',
    ),
    'process.env.ENSO_CLOUD_API_URL': stringify(process.env.ENSO_CLOUD_API_URL),
    'process.env.ENSO_CLOUD_SENTRY_DSN': stringify(process.env.ENSO_CLOUD_SENTRY_DSN),
    'process.env.ENSO_CLOUD_STRIPE_KEY': stringify(process.env.ENSO_CLOUD_STRIPE_KEY),
    'process.env.ENSO_CLOUD_CHAT_URL': stringify(process.env.ENSO_CLOUD_CHAT_URL),
    'process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID': stringify(
      process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID,
    ),
    'process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID': stringify(
      process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
    ),
    'process.env.ENSO_CLOUD_COGNITO_DOMAIN': stringify(process.env.ENSO_CLOUD_COGNITO_DOMAIN),
    'process.env.ENSO_CLOUD_COGNITO_REGION': stringify(process.env.ENSO_CLOUD_COGNITO_REGION),
    'process.env.ENSO_CLOUD_GOOGLE_ANALYTICS_TAG': stringify(
      process.env.ENSO_CLOUD_GOOGLE_ANALYTICS_TAG,
    ),
    'process.env.ENSO_CLOUD_DASHBOARD_VERSION': stringify(process.env.ENSO_CLOUD_DASHBOARD_VERSION),
    'process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH': stringify(
      process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH,
    ),
    'process.env.ENSO_CLOUD_ENSO_HOST': stringify(
      process.env.ENSO_CLOUD_ENSO_HOST ?? 'https://ensoanalytics.com',
    ),
  }
}

const DUMMY_DEFINES = {
  'process.env.NODE_ENV': 'production',
  'process.env.ENSO_CLOUD_ENVIRONMENT': 'production',
  'process.env.ENSO_CLOUD_API_URL': 'https://mock',
  'process.env.ENSO_CLOUD_SENTRY_DSN':
    'https://aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa@o0000000000000000.ingest.sentry.io/0000000000000000',
  'process.env.ENSO_CLOUD_STRIPE_KEY': '',
  'process.env.ENSO_CLOUD_CHAT_URL': '',
  'process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID': '',
  'process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID': '',
  'process.env.ENSO_CLOUD_COGNITO_DOMAIN': '',
  'process.env.ENSO_CLOUD_COGNITO_REGION': '',
  'process.env.ENSO_CLOUD_DASHBOARD_VERSION': '0.0.1-testing',
  'process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH': 'abcdef0',
}

/** Load test environment variables, useful for when the Cloud backend is mocked or unnecessary. */
export function loadTestEnvironmentVariables() {
  for (const [k, v] of Object.entries(DUMMY_DEFINES)) {
    process.env[k.replace(/^process[.]env[.]/, '')] = v
  }
}
