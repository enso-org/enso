/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('sign up flow', async ({ page }) => {
  const api = await actions.mockApi({ page })
  api.setCurrentUser(null)
  await page.goto('/')

  const email = 'example.email+1234@testing.org'
  const name = 'a custom user name'

  // These values should be different, otherwise the email and name may come from the defaults.
  test.expect(email).not.toStrictEqual(api.defaultEmail)
  test.expect(name).not.toStrictEqual(api.defaultName)

  // Set username panel
  await actions.locateEmailInput(page).fill(email)
  await actions.locatePasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateLoginButton(page).click()
  await test.expect(actions.locateSetUsernamePanel(page)).toBeVisible()

  // Logged in, but account disabled
  await actions.locateUsernameInput(page).fill(name)
  await actions.locateSetUsernameButton(page).click()
  await test.expect(actions.locateUpgradeButton(page)).toBeVisible()
  await test.expect(actions.locateDriveView(page)).not.toBeVisible()

  // Logged in, and account enabled
  const currentUser = api.currentUser
  test.expect(currentUser).toBeDefined()
  if (currentUser != null) {
    // This is required because `UserOrOrganization` is `readonly`.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, no-restricted-syntax, no-extra-semi
    ;(currentUser as { isEnabled: boolean }).isEnabled = true
  }
  await actions.login({ page }, email)
  await test.expect(actions.locateUpgradeButton(page)).not.toBeVisible()
  await test.expect(actions.locateDriveView(page)).toBeVisible()

  test.expect(api.currentUser?.email, 'new user has correct email').toBe(email)
  test.expect(api.currentUser?.name, 'new user has correct name').toBe(name)
})
