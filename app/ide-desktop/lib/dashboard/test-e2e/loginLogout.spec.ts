/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

// =============
// === Tests ===
// =============

test.test('login and logout', async ({ page }) => {
    await api.mockApi(page)

    // After sign in
    await actions.login(page)
    await test.expect(actions.locateDriveView(page)).toBeVisible()
    await test.expect(actions.locateLoginButton(page)).not.toBeVisible()

    // After sign out
    await actions.locateUserMenuButton(page).click()
    await actions.locateSignOutButton(page).click()
    await test.expect(actions.locateDriveView(page)).not.toBeVisible()
    await test.expect(actions.locateLoginButton(page)).toBeVisible()
})
