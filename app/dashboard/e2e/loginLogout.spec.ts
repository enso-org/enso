/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =============
// === Tests ===
// =============

test.test('login and logout', ({ page }) =>
  actions
    .mockAll({ page })
    .login()
    .do(async (thePage) => {
      await actions.passAgreementsDialog({ page: thePage })
      await test.expect(actions.locateDriveView(thePage)).toBeVisible()
      await test.expect(actions.locateLoginButton(thePage)).not.toBeVisible()
    })
    .openUserMenu()
    .userMenu.logout()
    .do(async (thePage) => {
      await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
      await test.expect(actions.locateLoginButton(thePage)).toBeVisible()
    }),
)
