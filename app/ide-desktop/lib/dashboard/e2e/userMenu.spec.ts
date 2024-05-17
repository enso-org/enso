/** @file Test the user menu. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('user menu', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions.openUserMenu().do(async thePage => {
      await test.expect(actions.locateUserMenu(thePage)).toBeVisible()
    })
  )
)

test.test('download app', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions.openUserMenu().do(async thePage => {
      // TODO: the "download app" button should probably not be a global.
      await actions.locateUserMenuButton(thePage).click()
      const downloadPromise = thePage.waitForEvent('download')
      await actions.locateDownloadAppButton(thePage).click()
      const download = await downloadPromise
      await download.cancel()
      test.expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
    })
  )
)
