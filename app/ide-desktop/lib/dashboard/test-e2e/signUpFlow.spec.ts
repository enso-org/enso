/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as apiModule from './api'

// =============
// === Tests ===
// =============

test.test('sign up flow', async ({ page }) => {
    await page.goto('/')
    const api = await apiModule.mockApi(page)
    api.setCurrentUser(null)

    // This SHOULD NOT be the same as the default email.
    const email = 'example.email+1234@testing.org'
    const name = 'user name'

    // Screenshot #1: Set username panel
    await actions.locateEmailInput(page).fill(email)
    await actions.locatePasswordInput(page).fill(actions.VALID_PASSWORD)
    await actions.locateLoginButton(page).click()
    await test.expect(actions.locateSetUsernamePanel(page)).toHaveScreenshot()

    // Screenshot #2: Logged in, but account disabled
    await actions.locateUsernameInput(page).fill(name)
    await actions.locateSetUsernameButton(page).click()
    await test.expect(page).toHaveScreenshot()

    const currentUser = api.currentUser
    if (currentUser != null) {
        currentUser.isEnabled = true
    }
    await test.expect(page).toHaveScreenshot()

    test.expect(api.currentUser?.email, 'new user has correct email').toBe(email)
    test.expect(api.currentUser?.name, 'new user has correct name').toBe(name)
})
