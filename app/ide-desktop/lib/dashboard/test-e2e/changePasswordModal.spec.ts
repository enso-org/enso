/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('change password modal', async ({ page }) => {
    await api.mockApi(page)
    await actions.login(page)

    // Screenshot #1: Change password modal
    await actions.locateUserMenuButton(page).click()
    await actions.locateChangePasswordButton(page).click()
    await test.expect(actions.locateChangePasswordModal(page)).toHaveScreenshot()

    // Screenshot #2: Invalid old password
    await actions.locateOldPasswordInput(page).fill(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid old password'
    ).toBe(false)
    await actions.locateResetButton(page).click()

    // Screenshot #3: Invalid new password
    await actions.locateOldPasswordInput(page).fill(actions.VALID_PASSWORD)
    await actions.locateNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid new password'
    ).toBe(false)
    await actions.locateResetButton(page).click()

    // Screenshot #4: Invalid "confirm new password"
    await actions.locateNewPasswordInput(page).fill(actions.VALID_PASSWORD)
    await actions.locateConfirmNewPasswordInput(page).fill(actions.INVALID_PASSWORD)
    test.expect(
        await page.evaluate(() => document.querySelector('form')?.checkValidity()),
        'form should reject invalid "confirm new password"'
    ).toBe(false)
    await actions.locateResetButton(page).click()

    // Screenshot #5: After form submission
    await actions.locateConfirmNewPasswordInput(page).fill(actions.VALID_PASSWORD)
    await actions.locateResetButton(page).click()
    await test.expect(actions.locateChangePasswordModal(page)).not.toBeAttached()
})
