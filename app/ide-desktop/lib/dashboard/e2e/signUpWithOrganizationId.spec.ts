/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =============
// === Tests ===
// =============

// Note: This does not check that the organization ID is sent in the correct format for the backend.
// It only checks that the organization ID is sent in certain places.
test.test('sign up with organization id', async ({ page }) => {
  await page.goto('/')
  await page.waitForLoadState('domcontentloaded')
  const organizationId = 'some testing organization id'
  await page.goto(
    '/registration?' + new URLSearchParams([['organization_id', organizationId]]).toString()
  )
  const api = await actions.mockApi({ page })
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

  test.expect(api.currentUser?.id, 'new user has correct organization id').toBe(organizationId)
})
