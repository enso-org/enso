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

/** Find a "set username" button (if any) on the current page. */
export function locateSetUsernameButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Set Username' })
}

/** Find a "delete" button (if any) on the current page. */
export function locateDeleteButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Delete' })
}

/** Find a button to open the editor (if any) on the current page. */
export function locatePlayOrOpenProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Open in editor')
}

/** Find a button to close the project (if any) on the current page. */
export function locateStopProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Stop execution')
}

// === Context menu buttons ===

/** Find an "open" button (if any) on the current page. */
export function locateOpenButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Open' })
}

/** Find an "upload to cloud" button (if any) on the current page. */
export function locateUploadToCloudButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Upload To Cloud' })
}

/** Find a "rename" button (if any) on the current page. */
export function locateRenameButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Rename' })
}

/** Find a "snapshot" button (if any) on the current page. */
export function locateSnapshotButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Snapshot' })
}

/** Find a "move to trash" button (if any) on the current page. */
export function locateMoveToTrashButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Move To Trash' })
}

/** Find a "move all to trash" button (if any) on the current page. */
export function locateMoveAllToTrashButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Move All To Trash' })
}

/** Find a "share" button (if any) on the current page. */
export function locateShareButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Share' })
}

/** Find a "label" button (if any) on the current page. */
export function locateLabelButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Label' })
}

/** Find a "duplicate" button (if any) on the current page. */
export function locateDuplicateButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Duplicate' })
}

/** Find a "copy" button (if any) on the current page. */
export function locateCopyButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Copy' })
}

/** Find a "cut" button (if any) on the current page. */
export function locateCutButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Cut' })
}

/** Find a "download" button (if any) on the current page. */
export function locateDownloadButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Download' })
}

/** Find an "upload files" button (if any) on the current page. */
export function locateUploadFilesButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Upload Files' })
}

/** Find a "new project" button (if any) on the current page. */
export function locateNewProjectButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Project' })
}

/** Find a "new folder" button (if any) on the current page. */
export function locateNewFolderButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Folder' })
}

/** Find a "new data connector" button (if any) on the current page. */
export function locateNewDataConnectorButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Data Connector' })
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

/** Find a "confirm delete" modal (if any) on the current page. */
export function locateConfirmDeleteModal(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('confirm-delete-modal')
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

/** Find a set of context menus (if any) on the current page. */
export function locateContextMenus(page: test.Locator | test.Page) {
    return page.getByTestId('context-menus')
}

// =============
// === login ===
// =============

/** Perform a successful login. */
export async function login(
    page: test.Page,
    email = 'email@example.com',
    password = VALID_PASSWORD
) {
    await page.goto('/')
    await locateEmailInput(page).fill(email)
    await locatePasswordInput(page).fill(password)
    await locateLoginButton(page).click()
}

// ================
// === mockDate ===
// ================

/** A placeholder date for visual regression testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45'))

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
