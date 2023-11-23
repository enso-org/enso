/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as apiModule from './api'

// =============
// === Tests ===
// =============

test.test('sign up flow', async ({ page }) => {
    const api = await apiModule.mockApi(page)
    api.setCurrentUser(null)
    await page.goto('/')

    const email = 'example.email+1234@testing.org'
    const name = 'a custom user name'

    // These values should be different, otherwise the email and name may come from the defaults.
    test.expect(email).not.toStrictEqual(api.defaultEmail)
    test.expect(name).not.toStrictEqual(api.defaultName)

    // Screenshot #1: Set username panel
    await actions.locateEmailInput(page).fill(email)
    await actions.locatePasswordInput(page).fill(actions.VALID_PASSWORD)
    await actions.locateLoginButton(page).click()
    await test.expect(actions.locateSetUsernamePanel(page)).toHaveScreenshot()

    // Screenshot #2: Logged in, but account disabled
    await actions.locateUsernameInput(page).fill(name)
    await actions.locateSetUsernameButton(page).click()
    await test.expect(page).toHaveScreenshot()

    // Screenshot #3: Logged in, and account enabled
    const currentUser = api.currentUser
    test.expect(currentUser).toBeDefined()
    if (currentUser != null) {
        currentUser.isEnabled = true
    }
    await actions.login(page, email)
    await test.expect(page).toHaveScreenshot()

    test.expect(api.currentUser?.email, 'new user has correct email').toBe(email)
    test.expect(api.currentUser?.name, 'new user has correct name').toBe(name)
})
