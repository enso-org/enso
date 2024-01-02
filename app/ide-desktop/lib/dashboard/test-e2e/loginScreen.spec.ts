/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =============
// === Tests ===
// =============

test.test('login screen', async ({ page }) => {
    // Screenshot omitted - it is already taken by `loginLogout.spec.ts`.
    await page.goto('/')

    // Invalid email
    await actions.locateEmailInput(page).fill('invalid email')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid email'
    ).toBe(false)
    await actions.locateLoginButton(page).click()

    // Invalid password
    await actions.locateEmailInput(page).fill(actions.VALID_EMAIL)
    await actions.locatePasswordInput(page).fill(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should accept invalid password'
    ).toBe(true)
    await actions.locateLoginButton(page).click()
})
