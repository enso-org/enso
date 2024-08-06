/** @file Test that emails are preserved when navigating between auth pages. */
import * as test from '@playwright/test'
import { VALID_EMAIL, mockAll } from './actions'

test.test('preserve email input when changing pages', ({ page }) =>
  mockAll({ page })
    .fillEmail(VALID_EMAIL)
    .goToPage.register()
    .withEmailInput(async (emailInput) => {
      await test.expect(emailInput).toHaveText(VALID_EMAIL)
    })
    .fillEmail(`2${VALID_EMAIL}`)
    .goToPage.login()
    .withEmailInput(async (emailInput) => {
      await test.expect(emailInput).toHaveText(`2${VALID_EMAIL}`)
    }),
)
