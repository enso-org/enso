/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(({ page }) => actions.mockAll({ page }))

// =============
// === Tests ===
// =============

test.test('login screen', async ({ page }) => {
  // Invalid email
  await page.goto('/')
  await actions.locateEmailInput(page).fill('invalid email')
  await actions.locateLoginButton(page).click()

  // Invalid password
  await page.goto('/')
  await actions.locateEmailInput(page).fill(actions.VALID_EMAIL)
  await actions.locatePasswordInput(page).fill(actions.INVALID_PASSWORD)
  await actions.locateLoginButton(page).click()
})
