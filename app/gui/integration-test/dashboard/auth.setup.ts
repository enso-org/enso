import fs from 'node:fs'

import { test as setup } from '@playwright/test'

import { getAuthFilePath, mockAllAndLogin } from './actions'

setup('authenticate', ({ page }) => {
  setup.slow()
  const authFilePath = getAuthFilePath()
  setup.skip(fs.existsSync(authFilePath), 'Already authenticated')
  return mockAllAndLogin({ page })
})
