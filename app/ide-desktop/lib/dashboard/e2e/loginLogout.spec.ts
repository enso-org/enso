/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// Do not login in setup, because this test needs to test login.
test.test.beforeEach(actions.mockAll)

// =============
// === Tests ===
// =============

test.test('login and logout', async ({ page }) => {
  // After sign in
  await actions.login({ page })
  await test.expect(actions.locateDriveView(page)).toBeVisible()
  await test.expect(actions.locateLoginButton(page)).not.toBeVisible()

  // After sign out
  await actions.locateUserMenuButton(page).click()
  await actions.locateLogoutButton(page).click()
  await test.expect(actions.locateDriveView(page)).not.toBeVisible()
  await test.expect(actions.locateLoginButton(page)).toBeVisible()
})
