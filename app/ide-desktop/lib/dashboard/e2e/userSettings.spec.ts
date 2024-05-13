/** @file Test the user settings tab. */
import * as test from '@playwright/test'
import * as chai from 'chai'

import * as actions from './actions'

test.test('user settings', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const localActions = actions.settings.userAccount
  test.expect(api.currentUser()?.name).toBe(api.defaultName)

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another user-name'
  await nameInput.fill(newName)
  await nameInput.press('Enter')
  test.expect(api.currentUser()?.name).toBe(newName)
  test.expect(api.currentOrganization()?.name).not.toBe(newName)
})

test.test('change password form', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const localActions = actions.settings.changePassword

  await localActions.go(page)
  test.expect(api.currentPassword()).toBe(actions.VALID_PASSWORD)
  await localActions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
  await localActions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
  await test
    .expect(localActions.locateChangeButton(page), 'incomplete form should be rejected')
    .toBeDisabled()

  await test.test.step('invalid new password', async () => {
    await localActions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
    await localActions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
    await localActions.locateConfirmNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
    test
      .expect(
        await localActions
          .locateNewPasswordInput(page)
          .evaluate((element: HTMLInputElement) => element.validity.valid),
        'invalid new password should be rejected'
      )
      .toBe(false)
    await test
      .expect(localActions.locateChangeButton(page), 'invalid new password should be rejected')
      .toBeDisabled()
  })

  await test.test.step('invalid new password confirmation', async () => {
    await localActions.locateCurrentPasswordInput(page).fill(actions.VALID_PASSWORD)
    await localActions.locateNewPasswordInput(page).fill(actions.VALID_PASSWORD)
    await localActions.locateConfirmNewPasswordInput(page).fill(actions.VALID_PASSWORD + 'a')
    test
      .expect(
        await localActions
          .locateConfirmNewPasswordInput(page)
          .evaluate((element: HTMLInputElement) => element.validity.valid),
        'invalid new password confirmation should be rejected'
      )
      .toBe(false)
    await test
      .expect(
        localActions.locateChangeButton(page),
        'invalid new password confirmation should be rejected'
      )
      .toBeDisabled()
  })

  await test.test.step('successful password change', async () => {
    const newPassword = '1234!' + actions.VALID_PASSWORD
    await localActions.locateNewPasswordInput(page).fill(newPassword)
    await localActions.locateConfirmNewPasswordInput(page).fill(newPassword)
    await localActions.locateChangeButton(page).click()
    await test.expect(localActions.locateCurrentPasswordInput(page)).toHaveText('')
    await test.expect(localActions.locateNewPasswordInput(page)).toHaveText('')
    await test.expect(localActions.locateConfirmNewPasswordInput(page)).toHaveText('')
    test.expect(api.currentPassword()).toBe(newPassword)
  })
})

test.test('upload profile picture', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const localActions = actions.settings.profilePicture

  await localActions.go(page)
  const fileChooserPromise = page.waitForEvent('filechooser')
  await localActions.locateInput(page).click()
  const fileChooser = await fileChooserPromise
  const name = 'foo.png'
  const content = 'a profile picture'
  await fileChooser.setFiles([{ name, buffer: Buffer.from(content), mimeType: 'image/png' }])
  await test
    .expect(() => {
      test.expect(api.currentProfilePicture()).toEqual(content)
    })
    .toPass()
})
