/** @file Test the login flow. */
import { expect, test, type Page } from 'integration-test/base'

import { TEXT } from '../actions'

/** Find a "login" button.on the current locator. */
function locateLoginButton(page: Page) {
  return page.getByRole('button', { name: TEXT.login, exact: true }).getByText(TEXT.login)
}

/** Find a drive view. */
function locateDriveView(page: Page) {
  // This has no identifying features.
  return page.getByTestId('drive-view')
}

// Reset storage state for this file to avoid being authenticated
test.use({ storageState: { cookies: [], origins: [] } })

test('login and logout', async ({ loginPage }) => {
  await loginPage
    .login()
    .withDriveView(async (driveView) => {
      await expect(driveView).toBeVisible()
    })
    .do(async (thePage) => {
      await expect(locateLoginButton(thePage)).toBeHidden()
    })
    .openUserMenu()
    .userMenu.logout()
    .do(async (thePage) => {
      await expect(locateDriveView(thePage)).toBeHidden()
      await expect(locateLoginButton(thePage)).toBeVisible()
    })
})
