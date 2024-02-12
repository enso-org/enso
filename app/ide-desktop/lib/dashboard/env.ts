/** @file Read environment variables from a file. */
import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as url from 'node:url'

// ===============
// === loadEnv ===
// ===============

/** Read environment variables from a file. */
export async function loadEnv() {
  const environment = process.env.ENSO_CLOUD_ENV ?? null
  const isProduction = environment == null || environment === 'production'
  const fileName = isProduction ? '.env' : `.${environment}.env`
  const filePath = path.join(url.fileURLToPath(new URL('.', import.meta.url)), fileName)
  try {
    const file = await fs.readFile(filePath, { encoding: 'utf-8' })
    const variables = Object.fromEntries(
      file.split('\n').flatMap(line => {
        if (/^\s*$|^.s*#/.test(line)) {
          return []
        } else {
          const [key = '', value = ''] = line.split('=', 2)
          return [[key, value]]
        }
      })
    )
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
