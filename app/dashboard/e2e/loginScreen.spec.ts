/** @file Test the login flow. */
import * as test from '@playwright/test'

import {
  INVALID_PASSWORD,
  mockAll,
  passTermsAndConditionsDialog,
  VALID_EMAIL,
  VALID_PASSWORD,
} from './actions'

// =============
// === Tests ===
// =============

test.test('login screen', ({ page }) =>
  mockAll({ page })
    .loginThatShouldFail('invalid email', VALID_PASSWORD, 'Please enter a valid email address')
    // Technically it should not be allowed, but
    .login(VALID_EMAIL, INVALID_PASSWORD)
    .do(async (thePage) => {
      await passTermsAndConditionsDialog({ page: thePage })
    })
    .withDriveView(async (driveView) => {
      await test.expect(driveView).toBeVisible()
    }),
)
