/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =================
// === Constants ===
// =================

const EMAIL = 'example.email+1234@testing.org'
const NAME = 'a custom user name'
const ORGANIZATION_ID = 'some testing organization id'

// =============
// === Tests ===
// =============

// Note: This does not check that the organization ID is sent in the correct format for the backend.
// It only checks that the organization ID is sent in certain places.
test.test('sign up with organization id', async ({ page }) => {
  await page.goto('/')
  await page.waitForLoadState('domcontentloaded')
  await page.goto(
    '/registration?' + new URLSearchParams([['organization_id', ORGANIZATION_ID]]).toString()
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

  test
    .expect(api.currentUser()?.organizationId, 'new user has correct organization id')
    .toBe(ORGANIZATION_ID)
})

test.test('sign up without organization id', async ({ page }) => {
  await page.goto('/')
  await page.waitForLoadState('domcontentloaded')
  await page.goto('/registration')
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

  test
    .expect(api.currentUser()?.organizationId, 'new user has correct organization id')
    .toBe(api.defaultOrganizationId)
})

test.test('sign up flow', ({ page }) =>
  actions.mockAll({ page }).then(
    async ({ pageActions, api }) =>
      await pageActions
        .do(() => {
          api.setCurrentUser(null)

          // These values should be different, otherwise the email and name may come from the defaults.
          test.expect(EMAIL).not.toStrictEqual(api.defaultEmail)
          test.expect(NAME).not.toStrictEqual(api.defaultName)
        })
        .loginAsNewUser(EMAIL, actions.VALID_PASSWORD)
        .setUsername(NAME)
        .do(async thePage => {
          await test.expect(actions.locateUpgradeButton(thePage)).toBeVisible()
          await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
        })
        .do(() => {
          // Logged in, and account enabled
          const currentUser = api.currentUser()
          test.expect(currentUser).toBeDefined()
          if (currentUser != null) {
            // This is required because `UserOrOrganization` is `readonly`.
            // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, no-restricted-syntax, no-extra-semi
            ;(currentUser as { isEnabled: boolean }).isEnabled = true
          }
        })
        .openUserMenu()
        .userMenu.logout()
        .login(EMAIL, actions.VALID_PASSWORD)
        .do(async () => {
          await test.expect(actions.locateNotEnabledStub(page)).not.toBeVisible()
          await test.expect(actions.locateDriveView(page)).toBeVisible()
        })
        .do(() => {
          test.expect(api.currentUser()?.email, 'new user has correct email').toBe(EMAIL)
          test.expect(api.currentUser()?.name, 'new user has correct name').toBe(NAME)
        })
  )
)
