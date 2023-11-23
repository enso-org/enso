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
    return page.getByLabel('Email')
}

/** Find a password input (if any) on the current page. */
export function locatePasswordInput(page: test.Locator | test.Page) {
    return page.getByPlaceholder('Enter your password')
}

/** Find a "confirm password" input (if any) on the current page. */
export function locateConfirmPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Confirm password')
}

/** Find an "old password" input (if any) on the current page. */
export function locateOldPasswordInput(page: test.Locator | test.Page) {
    return page.getByLabel('Old password')
}

/** Find a "new password" input (if any) on the current page. */
export function locateNewPasswordInput(page: test.Locator | test.Page) {
    return page.getByPlaceholder('Enter your new password')
}

/** Find a "confirm new password" input (if any) on the current page. */
export function locateConfirmNewPasswordInput(page: test.Locator | test.Page) {
    return page.getByPlaceholder('Confirm your new password')
}

/** Find a "username" input (if any) on the current page. */
export function locateUsernameInput(page: test.Locator | test.Page) {
    return page.getByPlaceholder('Enter your username')
}

/** Find a "name" input for a "new label" modal (if any) on the current page. */
export function locateNewLabelModalNameInput(page: test.Locator | test.Page) {
    return locateNewLabelModal(page).getByLabel('Name')
}

/** Find all color radio button inputs for a "new label" modal (if any) on the current page. */
export function locateNewLabelModalColorButtons(page: test.Locator | test.Page) {
    return (
        locateNewLabelModal(page)
            .filter({ has: page.getByText('Color') })
            // The `radio` inputs are invisible, so they cannot be used in the locator.
            .getByRole('button')
    )
}

// === Button locators ===

/** Find a login button (if any) on the current page. */
export function locateLoginButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Login', exact: true }).getByText('Login')
}

/** Find a register button (if any) on the current page. */
export function locateRegisterButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Register' }).getByText('Register')
}

/** Find a reset button (if any) on the current page. */
export function locateResetButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Reset' }).getByText('Reset')
}

/** Find a user menu button (if any) on the current page. */
export function locateUserMenuButton(page: test.Locator | test.Page) {
    return page.getByAltText('Open user menu')
}

/** Find a change password button (if any) on the current page. */
export function locateChangePasswordButton(page: test.Locator | test.Page) {
    return page
        .getByRole('button', { name: 'Change your password' })
        .getByText('Change your password')
}

/** Find a "sign out" button (if any) on the current page. */
export function locateSignOutButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Sign out' }).getByText('Sign out')
}

/** Find a "set username" button (if any) on the current page. */
export function locateSetUsernameButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Set Username' }).getByText('Set Username')
}

/** Find a "delete" button (if any) on the current page. */
export function locateDeleteButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Delete' }).getByText('Delete')
}

/** Find a button to delete something (if any) on the current page. */
export function locateDeleteIcon(page: test.Locator | test.Page) {
    return page.getByAltText('Delete')
}

/** Find a "create" button (if any) on the current page. */
export function locateCreateButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Create' }).getByText('Create')
}

/** Find a button to open the editor (if any) on the current page. */
export function locatePlayOrOpenProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Open in editor')
}

/** Find a button to close the project (if any) on the current page. */
export function locateStopProjectButton(page: test.Locator | test.Page) {
    return page.getByAltText('Stop execution')
}

/** Find all labels in the labels panel (if any) on the current page. */
export function locateLabelsPanelLabels(page: test.Locator | test.Page) {
    return locateLabelsPanel(page).getByRole('button')
}

// === Context menu buttons ===

/** Find an "open" button (if any) on the current page. */
export function locateOpenButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Open' }).getByText('Open')
}

/** Find an "upload to cloud" button (if any) on the current page. */
export function locateUploadToCloudButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Upload To Cloud' }).getByText('Upload To Cloud')
}

/** Find a "rename" button (if any) on the current page. */
export function locateRenameButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Rename' }).getByText('Rename')
}

/** Find a "snapshot" button (if any) on the current page. */
export function locateSnapshotButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Snapshot' }).getByText('Snapshot')
}

/** Find a "move to trash" button (if any) on the current page. */
export function locateMoveToTrashButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Move To Trash' }).getByText('Move To Trash')
}

/** Find a "move all to trash" button (if any) on the current page. */
export function locateMoveAllToTrashButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Move All To Trash' }).getByText('Move All To Trash')
}

/** Find a "share" button (if any) on the current page. */
export function locateShareButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Share' }).getByText('Share')
}

/** Find a "label" button (if any) on the current page. */
export function locateLabelButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Label' }).getByText('Label')
}

/** Find a "duplicate" button (if any) on the current page. */
export function locateDuplicateButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Duplicate' }).getByText('Duplicate')
}

/** Find a "copy" button (if any) on the current page. */
export function locateCopyButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Copy' }).getByText('Copy')
}

/** Find a "cut" button (if any) on the current page. */
export function locateCutButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Cut' }).getByText('Cut')
}

/** Find a "download" button (if any) on the current page. */
export function locateDownloadButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Download' }).getByText('Download')
}

/** Find an "upload files" button (if any) on the current page. */
export function locateUploadFilesButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'Upload Files' }).getByText('Upload Files')
}

/** Find a "new project" button (if any) on the current page. */
export function locateNewProjectButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Project' }).getByText('New Project')
}

/** Find a "new folder" button (if any) on the current page. */
export function locateNewFolderButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Folder' }).getByText('New Folder')
}

/** Find a "new data connector" button (if any) on the current page. */
export function locateNewDataConnectorButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'New Data Connector' }).getByText('New Data Connector')
}

/** Find a "new label" button (if any) on the current page. */
export function locateNewLabelButton(page: test.Locator | test.Page) {
    return page.getByRole('button', { name: 'new label' }).getByText('new label')
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

/** Find a "new label" modal (if any) on the current page. */
export function locateNewLabelModal(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('new-label-modal')
}

/** Find a user menu (if any) on the current page. */
export function locateUserMenu(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('user-menu')
}

/** Find a "set username" panel (if any) on the current page. */
export function locateSetUsernamePanel(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('set-username-panel')
}

/** Find a set of context menus (if any) on the current page. */
export function locateContextMenus(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('context-menus')
}

/** Find a labels panel (if any) on the current page. */
export function locateLabelsPanel(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('labels')
}

/** Find a list of labels (if any) on the current page. */
export function locateLabelsList(page: test.Locator | test.Page) {
    // This has no identifying features.
    return page.getByTestId('labels-list')
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
