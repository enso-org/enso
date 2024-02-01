/** @file Various actions, locators, and constants used in end-to-end tests. */
import * as test from '@playwright/test'

import * as apiModule from './api'

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

/** Find a "name" input for an "upsert secret" modal (if any) on the current page. */
export function locateSecretNameInput(page: test.Locator | test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the name of the secret')
}

/** Find a "value" input for an "upsert secret" modal (if any) on the current page. */
export function locateSecretValueInput(page: test.Locator | test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the value of the secret')
}

/** Find a search bar input (if any) on the current page. */
export function locateSearchBarInput(page: test.Locator | test.Page) {
  return locateSearchBar(page).getByPlaceholder(
    'Type to search for projects, data connectors, users, and more.'
  )
}

/** Find the name column of the given assets table row. */
export function locateAssetRowName(locator: test.Locator) {
  return locator.getByTestId('asset-row-name')
}

// === Button locators ===

/** Find a toast close button (if any) on the current page. */
export function locateToastCloseButton(page: test.Locator | test.Page) {
  // There is no other simple way to uniquely identify this element.
  // eslint-disable-next-line no-restricted-properties
  return page.locator('.Toastify__close-button')
}

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
export function locateLogoutButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Logout' }).getByText('Logout')
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
  return (
    locateLabelsPanel(page)
      .getByRole('button')
      // The delete button is also a `button`.
      // eslint-disable-next-line no-restricted-properties
      .and(page.locator(':nth-child(1)'))
  )
}

/** Find a "home" button (if any) on the current page. */
export function locateHomeButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Home' }).getByText('Home')
}

/** Find a "trash" button (if any) on the current page. */
export function locateTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Trash' }).getByText('Trash')
}

/** Find a tick button (if any) on the current page. */
export function locateEditingTick(page: test.Locator | test.Page) {
  return page.getByAltText('Confirm Edit')
}

/** Find a cross button (if any) on the current page. */
export function locateEditingCross(page: test.Locator | test.Page) {
  return page.getByAltText('Cancel Edit')
}

/** Find labels in the "Labels" column of the assets table (if any) on the current page. */
export function locateAssetLabels(page: test.Locator | test.Page) {
  return page.getByTestId('asset-label')
}

/** Find a toggle for the "Labels" column (if any) on the current page. */
export function locateLabelsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Labels$/)
}

/** Find a toggle for the "Accessed by projects" column (if any) on the current page. */
export function locateAccessedByProjectsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Accessed by projects$/)
}

/** Find a toggle for the "Accessed data" column (if any) on the current page. */
export function locateAccessedDataColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Accessed data$/)
}

/** Find a toggle for the "Docs" column (if any) on the current page. */
export function locateDocsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Docs$/)
}

/** Find a button for the "Recent" category (if any) on the current page. */
export function locateRecentCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go To Recent')
}

/** Find a button for the "Home" category (if any) on the current page. */
export function locateHomeCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go To Homoe')
}

/** Find a button for the "Trash" category (if any) on the current page. */
export function locateTrashCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go To Trash')
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

/** Find a "restore from trash" button (if any) on the current page. */
export function locateRestoreFromTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Restore From Trash' }).getByText('Restore From Trash')
}

/** Find a "restore all from trash" button (if any) on the current page. */
export function locateRestoreAllFromTrashButton(page: test.Locator | test.Page) {
  return page
    .getByRole('button', { name: 'Restore All From Trash' })
    .getByText('Restore All From Trash')
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

/** Find a "paste" button (if any) on the current page. */
export function locatePasteButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Paste' }).getByText('Paste')
}

/** Find a "download" button (if any) on the current page. */
export function locateDownloadButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Download' }).getByText('Download')
}

/** Find a "download app" button (if any) on the current page. */
export function locateDownloadAppButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Download App' }).getByText('Download App')
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

/** Find a "new secret" button (if any) on the current page. */
export function locateNewSecretButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Secret' }).getByText('New Secret')
}

/** Find a "new data connector" button (if any) on the current page. */
export function locateNewDataConnectorButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Data Connector' }).getByText('New Data Connector')
}

/** Find a "new label" button (if any) on the current page. */
export function locateNewLabelButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'new label' }).getByText('new label')
}

/** Find an "upgrade" button (if any) on the current page. */
export function locateUpgradeButton(page: test.Locator | test.Page) {
  return page.getByRole('link', { name: 'Upgrade', exact: true }).getByText('Upgrade')
}

/** Find a "new folder" icon (if any) on the current page. */
export function locateNewFolderIcon(page: test.Locator | test.Page) {
  return page.getByAltText('New Folder')
}

/** Find a "new secret" icon (if any) on the current page. */
export function locateNewSecretIcon(page: test.Locator | test.Page) {
  return page.getByAltText('New Secret')
}

/** Find a "upload files" icon (if any) on the current page. */
export function locateUploadFilesIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Upload Files')
}

/** Find a "download files" icon (if any) on the current page. */
export function locateDownloadFilesIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Download Files')
}

/** Find an icon to open or close the asset panel (if any) on the current page. */
export function locateAssetPanelIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Open Asset Panel').or(page.getByAltText('Close Asset Panel'))
}

/** Find a list of tags in the search bar (if any) on the current page. */
export function locateSearchBarTags(page: test.Locator | test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-tag-names').getByRole('button')
}

/** Find a list of labels in the search bar (if any) on the current page. */
export function locateSearchBarLabels(page: test.Locator | test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-labels').getByRole('button')
}

/** Find a list of labels in the search bar (if any) on the current page. */
export function locateSearchBarSuggestions(page: test.Locator | test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-suggestion')
}

// === Icon locators ===

// These are specifically icons that are not also buttons.
// Icons that *are* buttons belong in the "Button locators" section.

/** Find a "sort ascending" icon (if any) on the current page. */
export function locateSortAscendingIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Sort Ascending')
}

/** Find a "sort descending" icon (if any) on the current page. */
export function locateSortDescendingIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Sort Descending')
}

// === Page locators ===

/** Find a "home page" icon (if any) on the current page. */
export function locateHomePageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Go to home page')
}

/** Find a "drive page" icon (if any) on the current page. */
export function locateDrivePageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Go to drive page')
}

/** Find an "editor page" icon (if any) on the current page. */
export function locateEditorPageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Go to editor page')
}

/** Find a "name" column heading (if any) on the current page. */
export function locateNameColumnHeading(page: test.Locator | test.Page) {
  return page.getByTitle('Sort by name').or(page.getByTitle('Stop sorting by name'))
}

/** Find a "modified" column heading (if any) on the current page. */
export function locateModifiedColumnHeading(page: test.Locator | test.Page) {
  return page
    .getByTitle('Sort by modification date')
    .or(page.getByTitle('Stop sorting by modification date'))
}

// === Container locators ===

/** Find a drive view (if any) on the current page. */
export function locateDriveView(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('drive-view')
}

/** Find a samples list (if any) on the current page. */
export function locateSamplesList(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('samples')
}

/** Find all samples list (if any) on the current page. */
export function locateSamples(page: test.Locator | test.Page) {
  // This has no identifying features.
  return locateSamplesList(page).getByRole('button')
}

/** Find a modal background (if any) on the current page. */
export function locateModalBackground(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('modal-background')
}

/** Find an editor container (if any) on the current page. */
export function locateEditor(page: test.Page) {
  // This is fine as this element is defined in `index.html`, rather than from React.
  // Using `data-testid` may be more correct though.
  // eslint-disable-next-line no-restricted-properties
  return page.locator('#root')
}

/** Find an assets table (if any) on the current page. */
export function locateAssetsTable(page: test.Page) {
  return locateDriveView(page).getByRole('table')
}

/** Find assets table rows (if any) on the current page. */
export function locateAssetRows(page: test.Page) {
  return locateAssetsTable(page).locator('tbody').getByRole('row')
}

/** Find the name column of the given asset row. */
export function locateAssetName(locator: test.Locator) {
  return locator.locator('> :nth-child(1)')
}

/** Find assets table rows that represent directories that can be expanded (if any)
 * on the current page. */
export function locateExpandableDirectories(page: test.Page) {
  return locateAssetRows(page).filter({ has: page.getByAltText('Expand') })
}

/** Find assets table rows that represent directories that can be collapsed (if any)
 * on the current page. */
export function locateCollapsibleDirectories(page: test.Page) {
  return locateAssetRows(page).filter({ has: page.getByAltText('Collapse') })
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

/** Find an "upsert secret" modal (if any) on the current page. */
export function locateUpsertSecretModal(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('upsert-secret-modal')
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

/** Find an asset panel (if any) on the current page. */
export function locateAssetPanel(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-panel')
}

/** Find a search bar (if any) on the current page. */
export function locateSearchBar(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-search-bar')
}

// === Content locators ===

/** Find an asset description in an asset panel (if any) on the current page. */
export function locateAssetPanelDescription(page: test.Locator | test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-description')
}

/** Find asset permissions in an asset panel (if any) on the current page. */
export function locateAssetPanelPermissions(page: test.Locator | test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-permissions').getByRole('button')
}

// ===============================
// === Visual layout utilities ===
// ===============================

/** Get the left side of the bounding box of an asset row. The locator MUST be for an asset row.
 * DO NOT assume the left side of the outer container will change. This means that it is NOT SAFE
 * to do anything with the returned values other than comparing them. */
export function getAssetRowLeftPx(locator: test.Locator) {
  return locator.evaluate(el => el.children[0]?.children[0]?.getBoundingClientRect().left ?? 0)
}

// ============================
// === expectPlaceholderRow ===
// ============================

/** A test assertion to confirm that there is only one row visible, and that row is the
 * placeholder row displayed when there are no assets to show. */
export async function expectPlaceholderRow(page: test.Page) {
  const assetRows = locateAssetRows(page)
  await test.test.step('Expect placeholder row', async () => {
    await test.expect(assetRows).toHaveCount(1)
    await test.expect(assetRows).toHaveText(/You have no files/)
  })
}

/** A test assertion to confirm that there is only one row visible, and that row is the
 * placeholder row displayed when there are no assets in Trash. */
export async function expectTrashPlaceholderRow(page: test.Page) {
  const assetRows = locateAssetRows(page)
  await test.test.step('Expect trash placeholder row', async () => {
    await test.expect(assetRows).toHaveCount(1)
    await test.expect(assetRows).toHaveText(/Your trash is empty/)
  })
}

// ==========================
// === Keyboard utilities ===
// ==========================

/** `Meta` (`Cmd`) on macOS, and `Control` on all other platforms. */
export async function modModifier(page: test.Page) {
  let userAgent = ''
  await test.test.step('Detect browser OS', async () => {
    userAgent = await page.evaluate(() => navigator.userAgent)
  })
  return /\bMac OS\b/i.test(userAgent) ? 'Meta' : 'Control'
}

/** Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
 * on all other platforms. */
export async function press(page: test.Page, keyOrShortcut: string) {
  if (/\bMod\b|\bDelete\b/.test(keyOrShortcut)) {
    let userAgent = ''
    await test.test.step('Detect browser OS', async () => {
      userAgent = await page.evaluate(() => navigator.userAgent)
    })
    // This should be `Meta` (`Cmd`) on macOS, and `Control` on all other systems
    const ctrlKey = /\bMac OS\b/i.test(userAgent) ? 'Meta' : 'Control'
    const deleteKey = /\bMac OS\b/i.test(userAgent) ? 'Backspace' : 'Delete'
    await page.keyboard.press(
      keyOrShortcut.replace(/\bMod\b/g, ctrlKey).replace(/\bDelete\b/, deleteKey)
    )
  } else {
    await page.keyboard.press(keyOrShortcut)
  }
}

// =============
// === login ===
// =============

/** Perform a successful login. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function login(
  { page }: MockParams,
  email = 'email@example.com',
  password = VALID_PASSWORD
) {
  await page.goto('/')
  await locateEmailInput(page).fill(email)
  await locatePasswordInput(page).fill(password)
  await locateLoginButton(page).click()
  await locateToastCloseButton(page).click()
}

// ================
// === mockDate ===
// ================

/** A placeholder date for visual regression testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45'))

/** Parameters for {@link mockDate}. */
interface MockParams {
  page: test.Page
}

/** Replace `Date` with a version that returns a fixed time. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
async function mockDate({ page }: MockParams) {
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

// ========================
// === mockIDEContainer ===
// ========================

/** Make the IDE container have a non-zero size. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockIDEContainer({ page }: MockParams) {
  await page.evaluate(() => {
    const ideContainer = document.getElementById('root')
    if (ideContainer) {
      ideContainer.style.height = '100vh'
      ideContainer.style.width = '100vw'
    }
  })
}

// ===============
// === mockApi ===
// ===============

// This is a function, even though it does not use function syntax.
// eslint-disable-next-line no-restricted-syntax
export const mockApi = apiModule.mockApi

// ===============
// === mockAll ===
// ===============

/** Set up all mocks, without logging in. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockAll({ page }: MockParams) {
  const api = await mockApi({ page })
  await mockDate({ page })
  await mockIDEContainer({ page })
  return { api }
}

// =======================
// === mockAllAndLogin ===
// =======================

/** Set up all mocks, and log in with dummy credentials. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockAllAndLogin({ page }: MockParams) {
  const mocks = await mockAll({ page })
  await login({ page })
  // This MUST run after login, otherwise the element's styles are reset when the browser
  // is navigated to another page.
  await mockIDEContainer({ page })
  return mocks
}
