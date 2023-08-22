/** @file Various actions, locators, and constants used in end-to-end tests. */
import * as test from '@playwright/test'

// =================
// === Constants ===
// =================

/** An example password that does not meet validation requirements. */
export const INVALID_PASSWORD = 'password'
/** An example password that meets validation requirements. */
export const VALID_PASSWORD = 'Password0!'

// ================
// === Locators ===
// ================

/** Find an email input (if any) on the current page. */
export function locateEmailInput(page: test.Page) {
    return page.getByLabel('E-Mail Address:')
}

/** Find a password input (if any) on the current page. */
export function locatePasswordInput(page: test.Page) {
    return page.getByLabel('Password:')
}

/** Find an "old password" input (if any) on the current page. */
export function locateOldPasswordInput(page: test.Page) {
    return page.getByLabel('Old Password:')
}

/** Find a "new password" input (if any) on the current page. */
export function locateNewPasswordInput(page: test.Page) {
    return page.getByLabel('New Password:', { exact: true })
}

/** Find a "confirm new password" input (if any) on the current page. */
export function locateConfirmNewPasswordInput(page: test.Page) {
    return page.getByLabel('Confirm New Password:')
}

/** Find a login button (if any) on the current page. */
export function locateLoginButton(page: test.Page) {
    return page.getByRole('button', { name: 'Login', exact: true })
}

/** Find a reset button (if any) on the current page. */
export function locateResetButton(page: test.Page) {
    return page.getByRole('button', { name: 'Reset' })
}

/** Find a user menu button (if any) on the current page. */
export function locateUserMenuButton(page: test.Page) {
    return page.getByAltText('Open user menu')
}

/** Find a change password button (if any) on the current page. */
export function locateChangePasswordButton(page: test.Page) {
    return page.getByRole('button', { name: 'Change your password' })
}

/** Find a sign out button (if any) on the current page. */
export function locateSignOutButton(page: test.Page) {
    return page.getByRole('button', { name: 'Sign out' })
}

/** Find a "change password" modal (if any) on the current page. */
export function locateChangePasswordModal(page: test.Page) {
    // This has no identifying features.
    return page.getByTestId('change-password-modal')
}

/** Find a user menu (if any) on the current page. */
export function locateUserMenu(page: test.Page) {
    // This has no identifying features.
    return page.getByTestId('user-menu')
}

// =============
// === login ===
// =============

/** Perform a successful login. */
export async function login(page: test.Page) {
    await page.goto('/')
    await locateEmailInput(page).fill('')
    await locateEmailInput(page).type('email@example.com')
    await locatePasswordInput(page).fill('')
    await locatePasswordInput(page).type(VALID_PASSWORD)
    await locateLoginButton(page).click()
}
