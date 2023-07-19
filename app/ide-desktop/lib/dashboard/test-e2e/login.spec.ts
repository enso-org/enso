/** @file Test the login flow. */

import * as test from '@playwright/test'

test.test('login flow', async ({ page }) => {
    await page.goto('/')
    await page.waitForFunction(() => document.fonts.check('13.5px "M PLUS 1"'))
    await test.expect(page).toHaveScreenshot()

    await page.type('#email', 'invalid email')
    await page.click('[type=submit]')
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid email'
    ).toBe(false)

    await page.type('#email', 'email@example.com')
    await page.type('#password', 'password')
    await page.click('[type=submit]')
})
