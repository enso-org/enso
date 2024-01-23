/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as apiModule from './api'

// =============
// === Tests ===
// =============

test.test('sign up without organization id', async ({ page }) => {
  await page.goto('/')
  await page.waitForLoadState('domcontentloaded')
  await page.goto('/registration')
  const api = await apiModule.mockApi(page)
  api.setCurrentUser(null)

  // Sign up
  await actions.locateEmailInput(page).fill(actions.VALID_EMAIL)
  await actions.locatePasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateConfirmPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateRegisterButton(page).click()

  // Log in
  await actions.locateEmailInput(page).fill(actions.VALID_EMAIL)
  await actions.locatePasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateLoginButton(page).click()

  // Set username
  await actions.locateUsernameInput(page).fill('arbitrary username')
  await actions.locateSetUsernameButton(page).click()

  test
    .expect(api.currentUser?.id, 'new user has correct organization id')
    .toBe(api.defaultOrganizationId)
})
