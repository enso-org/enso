/** @file Functions for managing app configuration. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

// ===============================
// === readEnvironmentFromFile ===
// ===============================

/** Read environment variables from a file based on the `ENSO_CLOUD_ENV_FILE_NAME`
 * environment variable. Reads from `.env` if the variable is blank or absent.
 * DOES NOT override existing environment variables if the variable is absent. */
export async function readEnvironmentFromFile() {
    const environment = process.env.ENSO_CLOUD_ENV_FILE_NAME ?? null
    const isProduction = environment == null || environment === ''
    const fileName = isProduction ? '.env' : `.${environment}.env`
    const filePath = path.join(url.fileURLToPath(new URL('../../..', import.meta.url)), fileName)
    try {
        const file = await fs.readFile(filePath, { encoding: 'utf-8' })
        // eslint-disable-next-line jsdoc/valid-types
        /** @type {readonly (readonly [string, string])[]} */
        let entries = file.split('\n').flatMap(line => {
            if (/^\s*$|^.s*#/.test(line)) {
                return []
            } else {
                const [key = '', value = ''] = line.split('=', 2)
                return [[key, value]]
            }
        })
        if (environment == null) {
            entries = entries.filter(kv => {
                const [k] = kv
                return process.env[k] == null
            })
        }
        const variables = Object.fromEntries(entries)
        Object.assign(process.env, variables)
    } catch (error) {
        if (isProduction) {
            console.warn('Could not load `.env` file; disabling cloud backend.')
            console.error(error)
            return
        } else {
            throw error
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
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export function getDefines(serverPort = 8080) {
    return {
        /* eslint-disable @typescript-eslint/naming-convention */
        'process.env.ENSO_CLOUD_REDIRECT': stringify(
            // The actual environment variable does not necessarily exist.
            // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
            process.env.ENSO_CLOUD_REDIRECT ?? `http://localhost:${serverPort}`
        ),
        'process.env.ENSO_CLOUD_ENVIRONMENT': stringify(
            // The actual environment variable does not necessarily exist.
            // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
            process.env.ENSO_CLOUD_ENVIRONMENT ?? 'production'
        ),
        'process.env.ENSO_CLOUD_API_URL': stringify(process.env.ENSO_CLOUD_API_URL),
        'process.env.ENSO_CLOUD_SENTRY_DSN': stringify(process.env.ENSO_CLOUD_SENTRY_DSN),
        'process.env.ENSO_CLOUD_STRIPE_KEY': stringify(process.env.ENSO_CLOUD_STRIPE_KEY),
        'process.env.ENSO_CLOUD_CHAT_URL': stringify(process.env.ENSO_CLOUD_CHAT_URL),
        'process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID': stringify(
            process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID
        ),
        'process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID': stringify(
            process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID
        ),
        'process.env.ENSO_CLOUD_COGNITO_DOMAIN': stringify(process.env.ENSO_CLOUD_COGNITO_DOMAIN),
        'process.env.ENSO_CLOUD_COGNITO_REGION': stringify(process.env.ENSO_CLOUD_COGNITO_REGION),
        /* eslint-enable @typescript-eslint/naming-convention */
    }
}
