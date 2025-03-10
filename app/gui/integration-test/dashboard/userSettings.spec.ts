/** @file Test the user settings tab. */
import { expect, test } from '@playwright/test'

import { INVALID_PASSWORD, TEXT, VALID_PASSWORD, mockAllAndLogin } from './actions'

const NEW_USERNAME = 'another user-name'
const NEW_PASSWORD = '1234!' + VALID_PASSWORD
const PROFILE_PICTURE_FILENAME = 'foo.png'
const PROFILE_PICTURE_CONTENT = 'a profile picture'
const PROFILE_PICTURE_MIMETYPE = 'image/png'

test('user settings', ({ page }) =>
  mockAllAndLogin({ page })
    .do((_, { api }) => {
      expect(api.currentUser()?.name).toBe(api.defaultName)
    })
    .goToPage.settings()
    .accountForm()
    .fillName(NEW_USERNAME)
    .save()
    .do((_, { api }) => {
      expect(api.currentUser()?.name).toBe(NEW_USERNAME)
      expect(api.currentOrganization()?.name).not.toBe(NEW_USERNAME)
    }))

test('change password form', ({ page }) =>
  mockAllAndLogin({ page })
    .do((_, { api }) => {
      expect(api.currentPassword()).toBe(VALID_PASSWORD)
    })
    .goToPage.settings()
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(INVALID_PASSWORD)
    .fillConfirmNewPassword(INVALID_PASSWORD)
    .save()
    .step('Invalid new password should fail', async (page) => {
      await expect(page.getByTestId('error')).toHaveText(TEXT.passwordValidationError)
    })
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(VALID_PASSWORD)
    .fillConfirmNewPassword(VALID_PASSWORD + 'a')
    .save()
    .step('Invalid new password confirmation should fail', async (page) => {
      await expect(page.getByTestId('error')).toHaveText(TEXT.passwordMismatchError)
    })
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(NEW_PASSWORD)
    .fillConfirmNewPassword(NEW_PASSWORD)
    .save()
    // TODO: consider checking that password inputs are now empty.
    .step('Password change should be successful', (_, { api }) => {
      expect(api.currentPassword()).toBe(NEW_PASSWORD)
    }))

test('upload profile picture', ({ page }) =>
  mockAllAndLogin({ page })
    .goToPage.settings()
    .uploadProfilePicture(
      PROFILE_PICTURE_FILENAME,
      PROFILE_PICTURE_CONTENT,
      PROFILE_PICTURE_MIMETYPE,
    )
    .step('Profile picture should be updated', async (_, { api }) => {
      await expect(() => {
        expect(api.currentProfilePicture()).toEqual(PROFILE_PICTURE_CONTENT)
      }).toPass()
    }))
