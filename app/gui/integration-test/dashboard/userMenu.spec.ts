/** @file Test the user menu. */
import { expect, test } from 'integration-test/base'

import { TEXT } from '../actions'

test('user menu', ({ drivePage }) =>
  drivePage.openUserMenu().do(async (thePage) => {
    await expect(thePage.getByLabel(TEXT.userMenuLabel).locator('visible=true')).toBeVisible()
  }))

test('download app', ({ drivePage, cloudApi }) => {
  cloudApi.setFeatureFlags({ enableLocalBackend: false })
  drivePage.openUserMenu().userMenu.downloadApp(async (download) => {
    await download.cancel()
    expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
  })
})
