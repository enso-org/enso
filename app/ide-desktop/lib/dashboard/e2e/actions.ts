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

/** Find an email input (if any). */
export function locateEmailInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your email')
}

/** Find a password input (if any). */
export function locatePasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your password')
}

/** Find a "confirm password" input (if any). */
export function locateConfirmPasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Confirm your password')
}

/** Find a "current password" input (if any). */
export function locateCurrentPasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your current password')
}

/** Find a "new password" input (if any). */
export function locateNewPasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your new password')
}

/** Find a "confirm new password" input (if any). */
export function locateConfirmNewPasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Confirm your new password')
}

/** Find a "username" input (if any). */
export function locateUsernameInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your username')
}

/** Find a "name" input for a "new label" modal (if any). */
export function locateNewLabelModalNameInput(page: test.Page) {
  return locateNewLabelModal(page).getByLabel('Name')
}

/** Find all color radio button inputs for a "new label" modal (if any). */
export function locateNewLabelModalColorButtons(page: test.Page) {
  return (
    locateNewLabelModal(page)
      .filter({ has: page.getByText('Color') })
      // The `radio` inputs are invisible, so they cannot be used in the locator.
      .getByRole('button')
  )
}

/** Find a "name" input for an "upsert secret" modal (if any). */
export function locateSecretNameInput(page: test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the name of the secret')
}

/** Find a "value" input for an "upsert secret" modal (if any). */
export function locateSecretValueInput(page: test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the value of the secret')
}

/** Find a search bar input (if any). */
export function locateSearchBarInput(page: test.Page) {
  return locateSearchBar(page).getByPlaceholder(
    'Type to search for projects, Data Links, users, and more.'
  )
}

/** Find the name column of the given assets table row. */
export function locateAssetRowName(locator: test.Locator) {
  return locator.getByTestId('asset-row-name')
}

// === Button locators ===

/** Find a toast close button (if any). */
export function locateToastCloseButton(page: test.Locator | test.Page) {
  // There is no other simple way to uniquely identify this element.
  // eslint-disable-next-line no-restricted-properties
  return page.locator('.Toastify__close-button')
}

/** Find a "login" button (if any). */
export function locateLoginButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Login', exact: true }).getByText('Login')
}

/** Find a "register" button (if any). */
export function locateRegisterButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Register' }).getByText('Register')
}

/** Find a "reset" button (if any). */
export function locateResetButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Reset' }).getByText('Reset')
}

/** Find a "change" button (if any). */
export function locateChangeButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Change' }).getByText('Change')
}

/** Find an "open user menu" button (if any). */
export function locateUserMenuButton(page: test.Locator | test.Page) {
  return page.getByAltText('Open user menu').locator('visible=true')
}

/** Find a change password button (if any). */
export function locateChangePasswordButton(page: test.Locator | test.Page) {
  return page
    .getByRole('button', { name: 'Change your password' })
    .getByText('Change your password')
}

/** Find a "sign out" button (if any). */
export function locateLogoutButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Logout' }).getByText('Logout')
}

/** Find a "set username" button (if any). */
export function locateSetUsernameButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Set Username' }).getByText('Set Username')
}

/** Find a "delete" button (if any). */
export function locateDeleteButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Delete' }).getByText('Delete').first()
}

/** Find a button to delete something (if any). */
export function locateDeleteIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Delete')
}

/** Find a "create" button (if any). */
export function locateCreateButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Create' }).getByText('Create')
}

/** Find a button to open the editor (if any). */
export function locatePlayOrOpenProjectButton(page: test.Locator | test.Page) {
  return page.getByAltText('Open in editor')
}

/** Find a button to close the project (if any). */
export function locateStopProjectButton(page: test.Locator | test.Page) {
  return page.getByAltText('Stop execution')
}

/** Find all labels in the labels panel (if any). */
export function locateLabelsPanelLabels(page: test.Page) {
  return (
    locateLabelsPanel(page)
      .getByRole('button')
      // The delete button is also a `button`.
      // eslint-disable-next-line no-restricted-properties
      .and(page.locator(':nth-child(1)'))
  )
}

/** Find a "home" button (if any). */
export function locateHomeButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Home' }).getByText('Home')
}

/** Find a "trash" button (if any). */
export function locateTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Trash' }).getByText('Trash')
}

/** Find a tick button (if any). */
export function locateEditingTick(page: test.Locator | test.Page) {
  return page.getByAltText('Confirm Edit')
}

/** Find a cross button (if any). */
export function locateEditingCross(page: test.Locator | test.Page) {
  return page.getByAltText('Cancel Edit')
}

/** Find labels in the "Labels" column of the assets table (if any). */
export function locateAssetLabels(page: test.Locator | test.Page) {
  return page.getByTestId('asset-label')
}

/** Find a toggle for the "Name" column (if any). */
export function locateNameColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Name$/)
}

/** Find a toggle for the "Modified" column (if any). */
export function locateModifiedColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Modified date column$/)
}

/** Find a toggle for the "Shared with" column (if any). */
export function locateSharedWithColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Shared with column$/)
}

/** Find a toggle for the "Labels" column (if any). */
export function locateLabelsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Labels column$/)
}

/** Find a toggle for the "Accessed by projects" column (if any). */
export function locateAccessedByProjectsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Accessed by projects column$/)
}

/** Find a toggle for the "Accessed data" column (if any). */
export function locateAccessedDataColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Accessed data column$/)
}

/** Find a toggle for the "Docs" column (if any). */
export function locateDocsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText(/^(?:Show|Hide) Docs column$/)
}

/** Find a button for the "Recent" category (if any). */
export function locateRecentCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go to Recent category')
}

/** Find a button for the "Home" category (if any). */
export function locateHomeCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go to Home category')
}

/** Find a button for the "Trash" category (if any). */
export function locateTrashCategory(page: test.Locator | test.Page) {
  return page.getByTitle('Go to Trash category')
}

/** Find a button for the cloud backend (if any). */
export function locateCloudBackendButton(page: test.Locator | test.Page) {
  return page.getByTitle('Switch to cloud drive')
}

/** Find a button for the local backend (if any). */
export function locateLocalBackendButton(page: test.Locator | test.Page) {
  return page.getByTitle('Switch to local drive')
}

// === Context menu buttons ===

/** Find an "open" button (if any). */
export function locateOpenButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Open' }).getByText('Open')
}

/** Find an "upload to cloud" button (if any). */
export function locateUploadToCloudButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Upload To Cloud' }).getByText('Upload To Cloud')
}

/** Find a "rename" button (if any). */
export function locateRenameButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Rename' }).getByText('Rename')
}

/** Find a "snapshot" button (if any). */
export function locateSnapshotButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Snapshot' }).getByText('Snapshot')
}

/** Find a "move to trash" button (if any). */
export function locateMoveToTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Move To Trash' }).getByText('Move To Trash')
}

/** Find a "move all to trash" button (if any). */
export function locateMoveAllToTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Move All To Trash' }).getByText('Move All To Trash')
}

/** Find a "restore from trash" button (if any). */
export function locateRestoreFromTrashButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Restore From Trash' }).getByText('Restore From Trash')
}

/** Find a "restore all from trash" button (if any). */
export function locateRestoreAllFromTrashButton(page: test.Locator | test.Page) {
  return page
    .getByRole('button', { name: 'Restore All From Trash' })
    .getByText('Restore All From Trash')
}

/** Find a "share" button (if any). */
export function locateShareButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Share' }).getByText('Share')
}

/** Find a "label" button (if any). */
export function locateLabelButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Label' }).getByText('Label')
}

/** Find a "duplicate" button (if any). */
export function locateDuplicateButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Duplicate' }).getByText('Duplicate')
}

/** Find a "copy" button (if any). */
export function locateCopyButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Copy' }).getByText('Copy')
}

/** Find a "cut" button (if any). */
export function locateCutButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Cut' }).getByText('Cut')
}

/** Find a "paste" button (if any). */
export function locatePasteButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Paste' }).getByText('Paste')
}

/** Find a "download" button (if any). */
export function locateDownloadButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Download' }).getByText('Download')
}

/** Find a "download app" button (if any). */
export function locateDownloadAppButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Download App' }).getByText('Download App')
}

/** Find an "upload files" button (if any). */
export function locateUploadFilesButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Upload Files' }).getByText('Upload Files')
}

/** Find a "new project" button (if any). */
export function locateNewProjectButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Project' }).getByText('New Project')
}

/** Find a "new folder" button (if any). */
export function locateNewFolderButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Folder' }).getByText('New Folder')
}

/** Find a "new secret" button (if any). */
export function locateNewSecretButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Secret' }).getByText('New Secret')
}

/** Find a "new data connector" button (if any). */
export function locateNewDataConnectorButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Data Connector' }).getByText('New Data Connector')
}

/** Find a "new label" button (if any). */
export function locateNewLabelButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'new label' }).getByText('new label')
}

/** Find an "upgrade" button (if any). */
export function locateUpgradeButton(page: test.Locator | test.Page) {
  return page.getByRole('link', { name: 'Upgrade', exact: true }).getByText('Upgrade')
}

/** Find a "new folder" icon (if any). */
export function locateNewFolderIcon(page: test.Locator | test.Page) {
  return page.getByAltText('New Folder')
}

/** Find a "new secret" icon (if any). */
export function locateNewSecretIcon(page: test.Locator | test.Page) {
  return page.getByAltText('New Secret')
}

/** Find a "upload files" icon (if any). */
export function locateUploadFilesIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Upload Files')
}

/** Find a "download files" icon (if any). */
export function locateDownloadFilesIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Download Files')
}

/** Find an icon to open or close the asset panel (if any). */
export function locateAssetPanelIcon(page: test.Locator | test.Page) {
  return page
    .getByAltText('Open Asset Panel')
    .or(page.getByAltText('Close Asset Panel'))
    .locator('visible=true')
}

/** Find a list of tags in the search bar (if any). */
export function locateSearchBarTags(page: test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-tag-names').getByRole('button')
}

/** Find a list of labels in the search bar (if any). */
export function locateSearchBarLabels(page: test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-labels').getByRole('button')
}

/** Find a list of labels in the search bar (if any). */
export function locateSearchBarSuggestions(page: test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-suggestion')
}

// === Icon locators ===

// These are specifically icons that are not also buttons.
// Icons that *are* buttons belong in the "Button locators" section.

/** Find a "sort ascending" icon (if any). */
export function locateSortAscendingIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Sort Ascending')
}

/** Find a "sort descending" icon (if any). */
export function locateSortDescendingIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Sort Descending')
}

// === Page locators ===

/** Find a "home page" icon (if any). */
export function locateHomePageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Home tab')
}

/** Find a "drive page" icon (if any). */
export function locateDrivePageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Drive tab')
}

/** Find an "editor page" icon (if any). */
export function locateEditorPageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Project tab')
}

/** Find a "settings page" icon (if any) on the current page. */
export function locateSettingsPageIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Settings tab')
}

/** Find a "name" column heading (if any). */
export function locateNameColumnHeading(page: test.Locator | test.Page) {
  return page.getByTitle('Sort by name').or(page.getByTitle('Stop sorting by name'))
}

/** Find a "modified" column heading (if any). */
export function locateModifiedColumnHeading(page: test.Locator | test.Page) {
  return page
    .getByTitle('Sort by modification date')
    .or(page.getByTitle('Stop sorting by modification date'))
}

// === Container locators ===

/** Find a drive view (if any). */
export function locateDriveView(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('drive-view')
}

/** Find a samples list (if any). */
export function locateSamplesList(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('samples')
}

/** Find all samples list (if any). */
export function locateSamples(page: test.Locator | test.Page) {
  // This has no identifying features.
  return locateSamplesList(page).getByRole('button')
}

/** Find a modal background (if any). */
export function locateModalBackground(page: test.Locator | test.Page) {
  // This has no identifying features.
  return page.getByTestId('modal-background')
}

/** Find an editor container (if any). */
export function locateEditor(page: test.Page) {
  // This is fine as this element is defined in `index.html`, rather than from React.
  // Using `data-testid` may be more correct though.
  // eslint-disable-next-line no-restricted-properties
  return page.locator('#app')
}

/** Find an assets table (if any). */
export function locateAssetsTable(page: test.Page) {
  return locateDriveView(page).getByRole('table')
}

/** Find assets table rows (if any). */
export function locateAssetRows(page: test.Page) {
  return locateAssetsTable(page).locator('tbody').getByRole('row')
}

/** Find the name column of the given asset row. */
export function locateAssetName(locator: test.Locator) {
  return locator.locator('> :nth-child(1)')
}

/** Find assets table rows that represent directories that can be expanded (if any)
 *. */
export function locateExpandableDirectories(page: test.Page) {
  return locateAssetRows(page).filter({ has: page.getByAltText('Expand') })
}

/** Find assets table rows that represent directories that can be collapsed (if any)
 *. */
export function locateCollapsibleDirectories(page: test.Page) {
  return locateAssetRows(page).filter({ has: page.getByAltText('Collapse') })
}

/** Find a "confirm delete" modal (if any). */
export function locateConfirmDeleteModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('confirm-delete-modal')
}

/** Find a "new label" modal (if any). */
export function locateNewLabelModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('new-label-modal')
}

/** Find an "upsert secret" modal (if any). */
export function locateUpsertSecretModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('upsert-secret-modal')
}

/** Find a user menu (if any). */
export function locateUserMenu(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('user-menu')
}

/** Find a "set username" panel (if any). */
export function locateSetUsernamePanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('set-username-panel')
}

/** Find a set of context menus (if any). */
export function locateContextMenus(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('context-menus')
}

/** Find a labels panel (if any). */
export function locateLabelsPanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('labels')
}

/** Find a list of labels (if any). */
export function locateLabelsList(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('labels-list')
}

/** Find an asset panel (if any). */
export function locateAssetPanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-panel')
}

/** Find a search bar (if any). */
export function locateSearchBar(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-search-bar')
}

/** Find an extra columns button panel (if any). */
export function locateExtraColumns(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('extra-columns')
}

/** Find a root directory dropzone (if any).
 * This is the empty space below the assets table, if it doesn't take up the whole screen
 * vertically. */
export function locateRootDirectoryDropzone(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('root-directory-dropzone')
}

// === Content locators ===

/** Find an asset description in an asset panel (if any). */
export function locateAssetPanelDescription(page: test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-description')
}

/** Find asset permissions in an asset panel (if any). */
export function locateAssetPanelPermissions(page: test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-permissions').getByRole('button')
}

/** Find the headers for all columns in the assets table (if it is present). */
export function locateAssetsColumnHeaders(page: test.Page) {
  return locateAssetsTable(page).locator('th')
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

// ===================================
// === expect functions for themes ===
// ===================================

/** A test assertion to confirm that the element has no background explicitly
 * set - not even an explicit transparent background. */
export async function expectNoBackground(locator: test.Locator) {
  await test.test.step('Expect no `bg-*`', async () => {
    await test.expect(locator).not.toHaveClass(/(?:^| )bg-/)
  })
}

/** A test assertion to confirm that the element has the class `selected`. */
export async function expectClassSelected(locator: test.Locator) {
  await test.test.step('Expect `selected`', async () => {
    await test.expect(locator).toHaveClass(/(?:^| )selected(?: |$)/)
  })
}

/** A test assertion to confirm that the element has the class `active`. */
export async function expectClassActive(locator: test.Locator) {
  await test.test.step('Expect `active`', async () => {
    await test.expect(locator).toHaveClass(/(?:^| )active(?: |$)/)
  })
}

/** A test assertion to confirm that the element does not have the class `active`. */
export async function expectNotClassActive(locator: test.Locator) {
  await test.test.step('Expect `active`', async () => {
    await test.expect(locator).not.toHaveClass(/(?:^| )active(?: |$)/)
  })
}

/** A test assertion to confirm that the element has `opacity > 0`. */
export async function expectNotTransparent(locator: test.Locator) {
  await test.test.step('expect.not.transparent', async () => {
    await test.expect
      .poll(() => locator.evaluate(element => getComputedStyle(element).opacity))
      .not.toBe('0')
  })
}

/** A test assertion to confirm that the element has `opacity === 0`. */
export async function expectTransparent(locator: test.Locator) {
  await test.test.step('expect.transparent', async () => {
    await test.expect
      .poll(() => locator.evaluate(element => getComputedStyle(element).opacity))
      .toBe('0')
  })
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

// =======================
// === Mouse utilities ===
// =======================

/** Click an asset row. The center must not be clicked as that is the button for adding a label. */
export async function clickAssetRow(assetRow: test.Locator) {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  await assetRow.click({ position: { x: 300, y: 16 } })
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
    const isMacOS = /\bMac OS\b/i.test(userAgent)
    const ctrlKey = isMacOS ? 'Meta' : 'Control'
    const deleteKey = isMacOS ? 'Backspace' : 'Delete'
    const shortcut = keyOrShortcut.replace(/\bMod\b/, ctrlKey).replace(/\bDelete\b/, deleteKey)
    await test.test.step(`Press '${shortcut}'`, () => page.keyboard.press(shortcut))
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
  readonly page: test.Page
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
    const ideContainer = document.getElementById('app')
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
