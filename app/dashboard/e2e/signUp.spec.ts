/** @file Test the login flow. */
import * as test from '@playwright/test'

import { INVALID_PASSWORD, mockAll, TEXT, VALID_EMAIL, VALID_PASSWORD } from './actions'

// =============
// === Tests ===
// =============

test.test('sign up without organization id', ({ page }) =>
  mockAll({ page })
    .goToPage.register()
    .registerThatShouldFail(
      'invalid email',
      VALID_PASSWORD,
      VALID_PASSWORD,
      TEXT.invalidEmailValidationError,
    )
    .registerThatShouldFail(
      VALID_EMAIL,
      INVALID_PASSWORD,
      INVALID_PASSWORD,
      TEXT.passwordValidationError,
    )
    .registerThatShouldFail(
      VALID_EMAIL,
      VALID_PASSWORD,
      INVALID_PASSWORD,
      TEXT.passwordMismatchError,
    )
    .register(),
)
