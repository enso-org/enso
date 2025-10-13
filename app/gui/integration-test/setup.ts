import { access, constants } from 'node:fs/promises'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { test as setup } from './base'

/** Get the path to the auth file. */
function getAuthFilePath() {
  const __dirname = dirname(fileURLToPath(import.meta.url))
  return join(__dirname, '../playwright/.auth/user.json')
}

setup.describe(async () => {
  const authFilePath = getAuthFilePath()
  const isAuthenticated = await access(authFilePath, constants.R_OK).then(
    () => true,
    () => false,
  )
  setup.skip(isAuthenticated, 'Already authenticated')
  setup('authenticate', async ({ page, loginPage }) => {
    await loginPage.login()
    await page.context().storageState({ path: authFilePath })
  })
})
