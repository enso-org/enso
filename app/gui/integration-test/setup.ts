import { access, constants } from 'node:fs/promises'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { test as setup } from './base'

const __dirname = dirname(fileURLToPath(import.meta.url))
const authFilePath = join(__dirname, '../playwright/.auth/user.json')

setup('authenticate', async ({ page, loginPage }) => {
  const isAuthenticated = await access(authFilePath, constants.R_OK).then(
    () => true,
    () => false,
  )

  if (!isAuthenticated) {
    await loginPage.login()
    await page.context().storageState({ path: authFilePath })
  }
})
