/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

// =============
// === Tests ===
// =============

test.test('login flow', async ({ page }) => {
    await api.mockApi(page)

    // Screenshot #1: Initial
    await page.goto('/')
    await test.expect(page).toHaveScreenshot()

    // Screenshot #2: Invalid email
    await actions.locateEmailInput(page).type('invalid email')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid email'
    ).toBe(false)
    await actions.locateLoginButton(page).click()
    await test.expect(page).toHaveScreenshot()

    // Screenshot #3: Invalid password
    await actions.locateEmailInput(page).fill('')
    await actions.locateEmailInput(page).type('email@example.com')
    await actions.locatePasswordInput(page).type(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid password'
    ).toBe(false)
    await actions.locateLoginButton(page).click()
    await test.expect(page).toHaveScreenshot()

    // Screenshot #4: After sign in
    await actions.login(page)
    await test.expect(page).toHaveScreenshot()

    // Screenshot #5: After sign out
    await actions.locateUserMenuButton(page).click()
    await actions.locateSignOutButton(page).click()
    await test.expect(page).toHaveScreenshot()
})
