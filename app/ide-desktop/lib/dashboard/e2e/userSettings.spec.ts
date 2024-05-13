/** @file Test the user settings tab. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('user settings', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const localActions = actions.settings.userAccount
  test.expect(api.currentUser?.name).toBe(api.defaultName)

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another user-name'
  await nameInput.fill(newName)
  await nameInput.press('Enter')
  test.expect(api.currentUser?.name).toBe(newName)
  test.expect(api.currentOrganization?.name).not.toBe(newName)
})
