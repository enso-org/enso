/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('change password modal', async ({ page }) => {
  // Change password modal
  await actions.locateUserMenuButton(page).click()
  await actions.locateChangePasswordButton(page).click()
  await test.expect(actions.locateChangePasswordModal(page)).toBeVisible()

  // Invalid old password
  await actions.locateOldPasswordInput(page).fill(actions.INVALID_PASSWORD)
  test
    .expect(
      await page.evaluate(() => document.querySelector('form')?.checkValidity()),
      'form should reject invalid old password'
    )
    .toBe(false)
  await actions.locateResetButton(page).click()

  // Invalid new password
  await actions.locateOldPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  test
    .expect(
      await page.evaluate(() => document.querySelector('form')?.checkValidity()),
      'form should reject invalid new password'
    )
    .toBe(false)
  await actions.locateResetButton(page).click()

  // Invalid new password confirmation
  await actions.locateNewPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateConfirmNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  test
    .expect(
      await page.evaluate(() => document.querySelector('form')?.checkValidity()),
      'form should reject invalid new password confirmation'
    )
    .toBe(false)
  await actions.locateResetButton(page).click()

  // After form submission
  await actions.locateConfirmNewPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateResetButton(page).click()
  await test.expect(actions.locateChangePasswordModal(page)).not.toBeAttached()
})
