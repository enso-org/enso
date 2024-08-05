/** @file Test the login flow. */
import * as test from '@playwright/test'

import { INVALID_PASSWORD, mockAll, passTermsAndConditionsDialog, VALID_EMAIL } from './actions'

// =============
// === Tests ===
// =============

test.test('login screen', async ({ page }) =>
  mockAll({ page })
    .loginThatShouldFail('invalid email')
    .login(VALID_EMAIL, INVALID_PASSWORD)
    .do(async (thePage) => {
      await passTermsAndConditionsDialog({ page: thePage })
    })
    .withDriveView(async (driveView) => {
      await test.expect(driveView).toBeVisible()
    }),
)
