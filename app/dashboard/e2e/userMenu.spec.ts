/** @file Test the user menu. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('user menu', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .openUserMenu()
    .do(async (thePage) => {
      await test.expect(actions.locateUserMenu(thePage)).toBeVisible()
    }),
)

test.test('download app', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    .openUserMenu()
    .userMenu.downloadApp(async (download) => {
      await download.cancel()
      test.expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
    }),
)
