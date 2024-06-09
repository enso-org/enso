/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('change password', async ({ page }) => {
  await actions.press(page, 'Mod+,')

  await actions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  await test
    .expect(actions.locateChangeButton(page), 'incomplete form should be rejected')
    .toBeDisabled()

  // Invalid new password
  await actions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  await actions.locateConfirmNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  test
    .expect(
      await actions
        .locateNewPasswordInput(page)
        .evaluate((element: HTMLInputElement) => element.validity.valid),
      'invalid new password should be rejected'
    )
    .toBe(false)
  await test
    .expect(actions.locateChangeButton(page), 'invalid new password should be rejected')
    .toBeDisabled()

  // Invalid new password confirmation
  await actions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateNewPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateConfirmNewPasswordInput(page).fill(actions.VALID_PASSWORD + 'a')
  test
    .expect(
      await actions
        .locateConfirmNewPasswordInput(page)
        .evaluate((element: HTMLInputElement) => element.validity.valid),
      'invalid new password confirmation should be rejected'
    )
    .toBe(false)
  await test
    .expect(
      actions.locateChangeButton(page),
      'invalid new password confirmation should be rejected'
    )
    .toBeDisabled()

  // After form submission
  await actions.locateConfirmNewPasswordInput(page).fill(actions.VALID_PASSWORD)
  await actions.locateChangeButton(page).click()
  await test.expect(actions.locateCurrentPasswordInput(page)).toHaveText('')
  await test.expect(actions.locateNewPasswordInput(page)).toHaveText('')
  await test.expect(actions.locateConfirmNewPasswordInput(page)).toHaveText('')
})
