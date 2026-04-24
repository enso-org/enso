/** @file Test the user settings tab. */
import { expect, test } from 'integration-test/base'

import { INVALID_PASSWORD, TEXT, VALID_PASSWORD } from '../actions'

const NEW_USERNAME = 'another user-name'
const NEW_PASSWORD = '1234!' + VALID_PASSWORD
const PROFILE_PICTURE_FILENAME = 'foo.png'
const PROFILE_PICTURE_CONTENT = 'a profile picture'
const PROFILE_PICTURE_MIMETYPE = 'image/png'

test('user settings', async ({ drivePage, cloudApi }) => {
  await drivePage.goToCategory
    .cloud()
    .do(() => {
      expect(cloudApi.currentUser()?.name).toBe(cloudApi.defaultName)
    })
    .goToPage.settings()
    .accountForm()
    .fillName(NEW_USERNAME)
    .save()
    .do(() => {
      expect(cloudApi.currentUser()?.name).toBe(NEW_USERNAME)
      expect(cloudApi.currentOrganization()?.name).not.toBe(NEW_USERNAME)
    })
})

test('change password form', async ({ drivePage, cloudApi }) => {
  await drivePage.goToCategory
    .cloud()
    .do(() => {
      expect(cloudApi.currentPassword()).toBe(VALID_PASSWORD)
    })
    .goToPage.settings()
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(INVALID_PASSWORD)
    .fillConfirmNewPassword(INVALID_PASSWORD)
    .save(false)
    .step('Invalid new password should fail', async (page) => {
      await expect(page.getByTestId('error')).toHaveText(TEXT.passwordValidationError)
    })
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(VALID_PASSWORD)
    .fillConfirmNewPassword(VALID_PASSWORD + 'a')
    .save(false)
    .step('Invalid new password confirmation should fail', async (page) => {
      await expect(page.getByTestId('error')).toHaveText(TEXT.passwordMismatchError)
    })
    .changePasswordForm()
    .fillCurrentPassword(VALID_PASSWORD)
    .fillNewPassword(NEW_PASSWORD)
    .fillConfirmNewPassword(NEW_PASSWORD)
    .save()
    // TODO: consider checking that password inputs are now empty.
    .step('Password change should be successful', () => {
      expect(cloudApi.currentPassword()).toBe(NEW_PASSWORD)
    })
})

test('upload profile picture', async ({ drivePage, cloudApi }) => {
  await drivePage.goToCategory
    .cloud()
    .goToPage.settings()
    .uploadProfilePicture(
      PROFILE_PICTURE_FILENAME,
      PROFILE_PICTURE_CONTENT,
      PROFILE_PICTURE_MIMETYPE,
    )
    .step('Profile picture should be updated', async () => {
      await expect(() => {
        expect(cloudApi.currentProfilePicture()).toEqual(PROFILE_PICTURE_CONTENT)
      }).toPass()
    })
})
