/** @file Read environment variables from a file. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

// ===============
// === loadEnv ===
// ===============

/** Read environment variables from a file based on the `ENSO_CLOUD_ENV_FILE_NAME`
 * environment variable. Reads from `.env` if the variable is blank or absent.
 * DOES NOT override existing environment variables if the variable is absent. */
export async function readEnvironmentFromFile() {
  const environment = process.env.ENSO_CLOUD_ENV_FILE_NAME ?? null
  const isProduction = environment == null || environment === ''
  const fileName = isProduction ? '.env' : `.${environment}.env`
  const filePath = path.join(url.fileURLToPath(new URL('.', import.meta.url)), fileName)
  try {
    const file = await fs.readFile(filePath, { encoding: 'utf-8' })
    let entries: readonly (readonly [string, string])[] = file.split('\n').flatMap(line => {
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
        // TypeScript thinks `process.env[k]` is `undefined` because of the index signature.
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        return process.env[k] == null
      })
    }
    const variables = Object.fromEntries(entries)
    Object.assign(process.env, variables)
  } catch (error) {
    if (isProduction) {
      console.warn('Could not load `.env` file; disabling cloud backend.')
      return
    } else {
      throw error
    }
  }
}
