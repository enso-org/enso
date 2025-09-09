import { existsSync } from 'node:fs'
import { getAuthFilePath } from './actions'
import { test as setup } from './base'

setup('authenticate', async ({ page, loginPage }) => {
  const authFilePath = getAuthFilePath()
  setup.skip(existsSync(authFilePath), 'Already authenticated')
  setup.slow()
  await loginPage.login()
  await page.context().storageState({ path: authFilePath })
})
