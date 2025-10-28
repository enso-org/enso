/** @file Test that emails are preserved when navigating between auth pages. */
import { expect, test } from 'integration-test/base'

import { VALID_EMAIL } from '../actions'

// Reset storage state for this file to avoid being authenticated
test.use({ storageState: { cookies: [], origins: [] } })

test('preserve email input when changing pages', async ({ loginPage }) => {
  await loginPage
    .fillEmail(VALID_EMAIL)
    .goToPage.register()
    .withEmailInput(async (emailInput) => {
      await expect(emailInput).toHaveValue(VALID_EMAIL)
    })
    .fillEmail(`2${VALID_EMAIL}`)
    .goToPage.login()
    .withEmailInput(async (emailInput) => {
      await expect(emailInput).toHaveValue(`2${VALID_EMAIL}`)
    })
    .fillEmail(`3${VALID_EMAIL}`)
    .goToPage.forgotPassword()
    .withEmailInput(async (emailInput) => {
      await expect(emailInput).toHaveValue(`3${VALID_EMAIL}`)
    })
    .fillEmail(`4${VALID_EMAIL}`)
    .goToPage.login()
    .withEmailInput(async (emailInput) => {
      await expect(emailInput).toHaveValue(`4${VALID_EMAIL}`)
    })
})
