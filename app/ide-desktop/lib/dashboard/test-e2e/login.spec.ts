/** @file Test the login flow. */

import * as test from '@playwright/test'

test.test('login flow', async ({ page }) => {
    await page.goto('/')
    await page.waitForFunction(() => document.fonts.ready)
    await test.expect(page).toHaveScreenshot()

    await page.type('#email', 'invalid email')
    await page.click('[type=submit]')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid email'
    ).toBe(false)

    await page.fill('#email', '')
    await page.type('#email', 'email@example.com')
    await page.type('#password', 'password')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid password'
    ).toBe(false)

    await page.fill('#password', '')
    await page.type('#password', 'Password0!')
    await page.click('[type=submit]')
    await test.expect(page).toHaveScreenshot()

    await page.waitForTimeout(3000)
    await test.expect(page).toHaveScreenshot()
})
