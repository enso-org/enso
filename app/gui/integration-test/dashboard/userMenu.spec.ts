/** @file Test the user menu. */
import { expect, test } from 'integration-test/base'

import { TEXT } from '../actions'

test('user menu', async ({ drivePage }) => {
  await drivePage.openUserMenu().do(async (thePage) => {
    await expect(thePage.getByLabel(TEXT.userMenuLabel).locator('visible=true')).toBeVisible()
  })
})

test.describe(() => {
  test.use({ featureFlags: { enableLocalBackend: false } })
  test('download app', async ({ drivePage }) => {
    await drivePage.openUserMenu().userMenu.downloadApp(async (download) => {
      await download.cancel()
      expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
    })
  })
})
