/** @file Test the user menu. */
import { expect, test } from '@playwright/test'

import { mockAllAndLogin, TEXT } from './actions'

test('user menu', ({ page }) =>
  mockAllAndLogin({ page, goToCloudFirst: false })
    .openUserMenu()
    .do(async (thePage) => {
      await expect(thePage.getByLabel(TEXT.userMenuLabel).locator('visible=true')).toBeVisible()
    }))

test('download app', ({ page }) =>
  mockAllAndLogin({
    page,
    goToCloudFirst: false,
    setupAPI: (api) => {
      api.setFeatureFlags({ enableLocalBackend: false })
    },
  })
    .openUserMenu()
    .userMenu.downloadApp(async (download) => {
      await download.cancel()
      expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
    }))
