/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

// =============
// === Tests ===
// =============

test.test('login and logout', async ({ page }) => {
  await api.mockApi(page)

  // Screenshot #1: After sign in
  await actions.login(page)
  await test.expect(page).toHaveScreenshot()

  // Screenshot #2: After sign out
  await actions.locateUserMenuButton(page).click()
  await actions.locateSignOutButton(page).click()
  await test.expect(page).toHaveScreenshot()
})
