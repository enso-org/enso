/** @file Read environment variables from a file. */
import * as fs from 'node:fs/promises'

// ===============
// === loadEnv ===
// ===============

/** Read environment variables from a file. */
export async function loadEnv() {
  const environment = process.env.ENSO_CLOUD_ENV ?? null
  const path = environment == null || environment === 'production' ? '.env' : `.${environment}.env`
  const file = await fs.readFile(path, { encoding: 'utf-8' })
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
}
