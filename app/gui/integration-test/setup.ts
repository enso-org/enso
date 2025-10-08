import { existsSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { test as setup } from './base'

/** Get the path to the auth file. */
function getAuthFilePath() {
  const __dirname = dirname(fileURLToPath(import.meta.url))
  return join(__dirname, '../playwright/.auth/user.json')
}

setup.describe(() => {
  const authFilePath = getAuthFilePath()
  setup.skip(existsSync(authFilePath), 'Already authenticated')
  setup('authenticate', async ({ page, loginPage }) => {
    await loginPage.login()
    await page.context().storageState({ path: authFilePath })
  })
})
