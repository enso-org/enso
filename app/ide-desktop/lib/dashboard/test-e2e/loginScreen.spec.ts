/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =============
// === Tests ===
// =============

test.test('login screen', async ({ page }) => {
    // Screenshot #1: Initial
    await page.goto('/')
    await test.expect(page).toHaveScreenshot()

    // Screenshot #2: Invalid email
    await actions.locateEmailInput(page).fill('invalid email')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid email'
    ).toBe(false)
    await actions.locateLoginButton(page).click()
    await test.expect(page).toHaveScreenshot()

    // Screenshot #3: Invalid password
    await actions.locateEmailInput(page).fill(actions.VALID_EMAIL)
    await actions.locatePasswordInput(page).type(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid password'
    ).toBe(false)
    await actions.locateLoginButton(page).click()
    await test.expect(page).toHaveScreenshot()
})
