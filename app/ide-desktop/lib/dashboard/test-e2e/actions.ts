/** @file Various actions, locators, and constants used in end-to-end tests. */
import type * as test from '@playwright/test'

// =================
// === Constants ===
// =================

/** An example password that does not meet validation requirements. */
export const INVALID_PASSWORD = 'password'
/** An example password that meets validation requirements. */
export const VALID_PASSWORD = 'Password0!'
/** An example valid email address. */
export const VALID_EMAIL = 'email@example.com'

// ================
// === Locators ===
// ================

// === Input locators ===

/** Find an email input (if any) on the current page. */
export function locateEmailInput(page: test.Locator | test.Page) {
    return page.getByLabel('E-Mail Address:')
}

/** Find a password input (if any) on the current page. */
export function locatePasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Password:', { exact: true })
}

/** Find a "confirm password" input (if any) on the current page. */
export function locateConfirmPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Confirm Password:')
}

/** Find an "old password" input (if any) on the current page. */
export function locateOldPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Old Password:')
}

/** Find a "new password" input (if any) on the current page. */
export function locateNewPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('New Password:', { exact: true })
}

/** Find a "confirm new password" input (if any) on the current page. */
export function locateConfirmNewPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Confirm New Password:')
}

/** Find a "username" input (if any) on the current page. */
export function locateUsernameInput(page: test.Locator | test.Page) {
    return page.getByPlaceholder('Username')
}

// === Button locators ===

/** Find a login button (if any) on the current page. */
export function locateLoginButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Login', exact: true })
}

/** Find a register button (if any) on the current page. */
export function locateRegisterButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Register' })
}

/** Find a reset button (if any) on the current page. */
export function locateResetButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Reset' })
}

/** Find a user menu button (if any) on the current page. */
export function locateUserMenuButton(page: test.Locator | test.Page) {
    return page.getByAltText('Open user menu')
}

/** Find a change password button (if any) on the current page. */
export function locateChangePasswordButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Change your password' })
}

/** Find a "sign out" button (if any) on the current page. */
export function locateSignOutButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Sign out' })
}

/** Find a "new project" button (if any) on the current page. */
export function locateNewProjectButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Project' })
}

/** Find a "set username" button (if any) on the current page. */
export function locateSetUsernameButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Set Username' })
}

/** Find a button to open the editor (if any) on the current page. */
export function locatePlayOrOpenProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Open in editor')
}

/** Find a button to close the project (if any) on the current page. */
export function locateStopProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Stop execution')
}

// === Container locators ===

/** Find a drive view (if any) on the current page. */
export function locateDriveView(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('drive-view')
}

/** Find an assets table (if any) on the current page. */
export function locateAssetsTable(page: test.Locator | test.Page) {
    return locateDriveView(page).getByRole('table')
}

/** Find assets table rows (if any) on the current page. */
export function locateAssetsTableRows(page: test.Locator | test.Page) {
    return locateAssetsTable(page).getByRole('row')
}

/** Find a "change password" modal (if any) on the current page. */
export function locateChangePasswordModal(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('change-password-modal')
}

/** Find a user menu (if any) on the current page. */
export function locateUserMenu(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('user-menu')
}

/** Find a "set username" panel (if any) on the current page. */
export function locateSetUsernamePanel(page: test.Locator | test.Page) {
    return page.getByTestId('set-username-panel')
}

// =============
// === login ===
// =============

/** Perform a successful login. */
export async function login(page: test.Page) {
    await page.goto('/')
    await locateEmailInput(page).fill('email@example.com')
    await locatePasswordInput(page).fill(VALID_PASSWORD)
    await locateLoginButton(page).click()
}

// ================
// === mockDate ===
// ================

/** A placeholder date for visual regresison testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45 UTC'))

/** Replace `Date` with a version that returns a fixed time. */
export async function mockDate(page: test.Page) {
    // https://github.com/microsoft/playwright/issues/6347#issuecomment-1085850728
    await page.addInitScript(`{
        Date = class extends Date {
            constructor(...args) {
            if (args.length === 0) {
                super(${MOCK_DATE});
            } else {
                super(...args);
            }
            }
        }
        const __DateNowOffset = ${MOCK_DATE} - Date.now();
        const __DateNow = Date.now;
        Date.now = () => __DateNow() + __DateNowOffset;
    }`)
}
