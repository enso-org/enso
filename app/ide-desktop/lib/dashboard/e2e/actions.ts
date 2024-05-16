/* eslint-disable @typescript-eslint/no-redeclare */
/** @file Various actions, locators, and constants used in end-to-end tests. */
import * as test from '@playwright/test'

import * as apiModule from './api'

/* eslint-disable @typescript-eslint/no-namespace */

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
  return page.getByPlaceholder('Enter your email')
}

/** Find a password input (if any) on the current page. */
export function locatePasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your password')
}

/** Find a "confirm password" input (if any) on the current page. */
export function locateConfirmPasswordInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Confirm your password')
}

/** Find a "username" input (if any) on the current page. */
export function locateUsernameInput(page: test.Locator | test.Page) {
  return page.getByPlaceholder('Enter your username')
}

/** Find a "name" input for a "new label" modal (if any) on the current page. */
export function locateNewLabelModalNameInput(page: test.Page) {
  return locateNewLabelModal(page).getByLabel('Name')
}

/** Find all color radio button inputs for a "new label" modal (if any) on the current page. */
export function locateNewLabelModalColorButtons(page: test.Page) {
  return (
    locateNewLabelModal(page)
      .filter({ has: page.getByText('Color') })
      // The `radio` inputs are invisible, so they cannot be used in the locator.
      .locator('label[data-rac]')
  )
}

/** Find a "name" input for an "upsert secret" modal (if any) on the current page. */
export function locateSecretNameInput(page: test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the name of the secret')
}

/** Find a "value" input for an "upsert secret" modal (if any) on the current page. */
export function locateSecretValueInput(page: test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder('Enter the value of the secret')
}

/** Find a search bar input (if any) on the current page. */
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

/** Find a toast close button (if any) on the current locator. */
export function locateToastCloseButton(page: test.Locator | test.Page) {
  // There is no other simple way to uniquely identify this element.
  // eslint-disable-next-line no-restricted-properties
  return page.locator('.Toastify__close-button')
}

/** Find a "login" button (if any) on the current locator. */
export function locateLoginButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Login', exact: true }).getByText('Login')
}

/** Find a "register" button (if any) on the current locator. */
export function locateRegisterButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Register' }).getByText('Register')
}

/** Find a user menu button (if any) on the current locator. */
export function locateUserMenuButton(page: test.Locator | test.Page) {
  return page.getByAltText('Open user menu').locator('visible=true')
}

/** Find a "sign out" button (if any) on the current locator. */
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
export function locateLabelsPanelLabels(page: test.Page) {
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

/** Find a toggle for the "Name" column (if any) on the current page. */
export function locateNameColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Name')
}

/** Find a toggle for the "Modified" column (if any) on the current page. */
export function locateModifiedColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Modified')
}

/** Find a toggle for the "Shared with" column (if any) on the current page. */
export function locateSharedWithColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Shared With')
}

/** Find a toggle for the "Labels" column (if any) on the current page. */
export function locateLabelsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Labels')
}

/** Find a toggle for the "Accessed by projects" column (if any) on the current page. */
export function locateAccessedByProjectsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Accessed By Projects')
}

/** Find a toggle for the "Accessed data" column (if any) on the current page. */
export function locateAccessedDataColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Accessed Data')
}

/** Find a toggle for the "Docs" column (if any) on the current page. */
export function locateDocsColumnToggle(page: test.Locator | test.Page) {
  return page.getByAltText('Docs')
}

/** Find a button for the "Recent" category (if any) on the current page. */
export function locateRecentCategory(page: test.Locator | test.Page) {
  return page.getByLabel('Recent').locator('visible=true')
}

/** Find a button for the "Home" category (if any) on the current page. */
export function locateHomeCategory(page: test.Locator | test.Page) {
  return page.getByLabel('Home').locator('visible=true')
}

/** Find a button for the "Trash" category (if any) on the current page. */
export function locateTrashCategory(page: test.Locator | test.Page) {
  return page.getByLabel('Trash').locator('visible=true')
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

/** Find a not enabled stub view (if any) on the current page. */
export function locateNotEnabledStub(page: test.Locator | test.Page) {
  return page.getByTestId('not-enabled-stub')
}

/** Find a "new folder" icon (if any) on the current page. */
export function locateNewFolderIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('New Folder') })
}

/** Find a "new secret" icon (if any) on the current page. */
export function locateNewSecretIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('New Secret') })
}

/** Find an "upload files" icon (if any) on the current page. */
export function locateUploadFilesIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('Import') })
}

/** Find a "download files" icon (if any) on the current page. */
export function locateDownloadFilesIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('Export') })
}

/** Find an icon to open or close the asset panel (if any) on the current page. */
export function locateAssetPanelIcon(page: test.Locator | test.Page) {
  return page.getByAltText('Asset Panel').locator('visible=true')
}

/** Find a list of tags in the search bar (if any) on the current page. */
export function locateSearchBarTags(page: test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-tag-names').getByRole('button')
}

/** Find a list of labels in the search bar (if any) on the current page. */
export function locateSearchBarLabels(page: test.Page) {
  return locateSearchBar(page).getByTestId('asset-search-labels').getByRole('button')
}

/** Find a list of labels in the search bar (if any) on the current page. */
export function locateSearchBarSuggestions(page: test.Page) {
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
  return page.getByRole('button').filter({ has: page.getByAltText('Home') })
}

/** Find a "drive page" icon (if any) on the current page. */
export function locateDrivePageIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('Catalog') })
}

/** Find a "settings page" icon (if any) on the current page. */
export function locateSettingsPageIcon(page: test.Locator | test.Page) {
  return page.getByRole('button').filter({ has: page.getByAltText('Settings') })
}

/** Find a "name" column heading (if any) on the current page. */
export function locateNameColumnHeading(page: test.Locator | test.Page) {
  return page.getByLabel('Sort by name').or(page.getByLabel('Stop sorting by name'))
}

/** Find a "modified" column heading (if any) on the current page. */
export function locateModifiedColumnHeading(page: test.Locator | test.Page) {
  return page
    .getByLabel('Sort by modification date')
    .or(page.getByLabel('Stop sorting by modification date'))
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
  return page.locator('#app')
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

/** Find a "confirm delete" modal (if any) on the current page. */
export function locateConfirmDeleteModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('confirm-delete-modal')
}

/** Find a "new label" modal (if any) on the current page. */
export function locateNewLabelModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('new-label-modal')
}

/** Find an "upsert secret" modal (if any) on the current page. */
export function locateUpsertSecretModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('upsert-secret-modal')
}

/** Find a "new user group" modal (if any) on the current page. */
export function locateNewUserGroupModal(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('new-user-group-modal')
}

/** Find a user menu (if any) on the current page. */
export function locateUserMenu(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('user-menu')
}

/** Find a "set username" panel (if any) on the current page. */
export function locateSetUsernamePanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('set-username-panel')
}

/** Find a set of context menus (if any) on the current page. */
export function locateContextMenus(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('context-menus')
}

/** Find a labels panel (if any) on the current page. */
export function locateLabelsPanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('labels')
}

/** Find a list of labels (if any) on the current page. */
export function locateLabelsList(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('labels-list')
}

/** Find an asset panel (if any) on the current page. */
export function locateAssetPanel(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-panel')
}

/** Find a search bar (if any) on the current page. */
export function locateSearchBar(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('asset-search-bar')
}

/** Find an extra columns button panel (if any) on the current page. */
export function locateExtraColumns(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('extra-columns')
}

/** Find a root directory dropzone (if any) on the current page.
 * This is the empty space below the assets table, if it doesn't take up the whole screen
 * vertically. */
export function locateRootDirectoryDropzone(page: test.Page) {
  // This has no identifying features.
  return page.getByTestId('root-directory-dropzone')
}

// === Content locators ===

/** Find an asset description in an asset panel (if any) on the current page. */
export function locateAssetPanelDescription(page: test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-description')
}

/** Find asset permissions in an asset panel (if any) on the current page. */
export function locateAssetPanelPermissions(page: test.Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-permissions').getByRole('button')
}

/** Navigate to the "home" page. */
async function goToHomePage(page: test.Page) {
  await test.test.step("Go to 'home' page", async () => {
    const homePageIcon = page.getByRole('button').filter({ has: page.getByAltText('Home') })
    return homePageIcon.click()
  })
}

/** Navigate to the "drive" page. */
async function goToDrivePage(page: test.Page) {
  await test.test.step("Go to 'drive' page", async () => {
    const drivePageIcon = page.getByRole('button').filter({ has: page.getByAltText('Catalog') })
    return drivePageIcon.click()
  })
}

/** Navigate to the settings page. */
async function goToEditorPage(page: test.Page) {
  await test.test.step("Go to 'editor' page", async () => {
    const editorPageIcon = page
      .getByRole('button')
      .filter({ has: page.getByAltText('Graph Editor') })
    return editorPageIcon.click()
  })
}

/** Navigate to the "settings" page. */
async function goToSettingsPage(page: test.Page) {
  await test.test.step("Go to 'settings' page", async () => {
    await press(page, 'Mod+,')
  })
}

/** Actions for going to a different page. */
interface GoToPageActions {
  readonly goToHomePage: () => ActionsStateActionsForState<'homePage'>
  readonly goToDrivePage: () => ActionsStateActionsForState<'drivePage'>
  readonly goToEditorPage: () => ActionsStateActionsForState<'editorPage'>
  readonly goToSettingsPage: () => ActionsStateActionsForState<'settingsPage'>
}

/** Generate actions for going to a different page. */
function goToPageActions(page: test.Page, promise: Promise<void>): GoToPageActions {
  return {
    goToHomePage: () =>
      actions(
        page,
        'homePage',
        {},
        thenStep(promise, 'Go to "home" page', () => goToHomePage(page))
      ),
    goToDrivePage: () =>
      actions(
        page,
        'drivePage',
        {},
        thenStep(promise, 'Go to "drive" page', () => goToDrivePage(page))
      ),
    goToEditorPage: () =>
      actions(
        page,
        'editorPage',
        {},
        thenStep(promise, 'Go to "editor" page', () => goToEditorPage(page))
      ),
    goToSettingsPage: () =>
      actions(
        page,
        'settingsPage',
        {},
        thenStep(promise, 'Go to "settings" page', () => goToSettingsPage(page))
      ),
  }
}

// =====================
// === State machine ===
// =====================

/** Valid states for the state machine. */
// eslint-disable-next-line @typescript-eslint/no-duplicate-type-constituents
type ActionsState = keyof ActionsStateActions & keyof ActionsStateData

/** State machine data. */
interface ActionsStateData {}

/** State machine actions. */
interface ActionsStateActions {}

/** Actions common to all {@link ActionsState}s. */
interface ActionsStateBase {
  readonly done: () => Promise<void>
}

/** Get the actual {@link ActionsStateActions} type for a specific {@link ActionsState}. */
type ActionsStateActionsForState<State extends ActionsState> = ActionsStateActions[State] &
  ActionsStateBase

const ACTIONS: {
  [K in ActionsState]: (
    page: test.Page,
    data: ActionsStateData[K],
    promise: Promise<void>
  ) => ActionsStateActions[K]
  // eslint-disable-next-line no-restricted-syntax
} = {} as never

// ======================
// === registerAction ===
// ======================

/** Register the set of actions possible for a given {@link ActionsState}. */
function registerAction<State extends ActionsState>(
  state: State,
  makeActions: (
    page: test.Page,
    data: ActionsStateData[State],
    promise: Promise<void>
  ) => ActionsStateActions[State]
) {
  // @ts-expect-error This is correct due to the type definition of `ACTIONS`.
  ACTIONS[state] = makeActions
}

// =======================
// === internalActions ===
// =======================

/** Return the actions for a given state machine state. */
function actions<State extends ActionsState>(
  page: test.Page,
  state: State,
  data: ActionsStateData[State],
  promise = Promise.resolve()
) {
  return { done: () => promise, ...ACTIONS[state](page, data, promise) }
}

/** Return an actions object for the home page. */
function loggedInActions(page: test.Page) {
  return actions(page, 'homePage', {})
}

/** Return an actions object for the login page. */
function loggedOutActions(page: test.Page) {
  // FIXME: switch to loginPage once implemented
  return actions(page, 'homePage', {})
}

// ================
// === homePage ===
// ================

/** Data for the "home page" state. */
interface HomePageData {}

/** Actions for the "home page" state. */
interface HomePageActions extends Omit<GoToPageActions, 'goToHomePage'> {
  readonly openDataLinkModal: () => ActionsStateActionsForState<'newDataLinkModal'>
}

/** State machine data. */
interface ActionsStateData {
  readonly homePage: HomePageData
}

/** State machine actions. */
interface ActionsStateActions {
  readonly homePage: HomePageActions
}

registerAction('homePage', (page, _data, promise) => ({
  ...goToPageActions(page, promise),
  openDataLinkModal: () => {
    const newPromise = thenStep(promise, 'Open "new data link" modal', async () => {
      await page
        .getByRole('button')
        .filter({ has: page.getByAltText('New Data Link') })
        .click()
    })
    return actions(page, 'newDataLinkModal', {}, newPromise)
  },
}))

// =================
// === drivePage ===
// =================

/** Data for the "drive page" state. */
interface DrivePageData {}

/** Actions for the "drive page" state. */
interface DrivePageActions extends Omit<GoToPageActions, 'goToDrivePage'> {}

/** State machine data. */
interface ActionsStateData {
  readonly drivePage: DrivePageData
}

/** State machine actions. */
interface ActionsStateActions {
  readonly drivePage: DrivePageActions
}

registerAction('drivePage', (page, _data, promise) => ({
  ...goToPageActions(page, promise),
}))

// ==================
// === editorPage ===
// ==================

/** Data for the "editor page" state. */
interface EditorPageData {}

/** Actions for the "editor page" state. */
interface EditorPageActions extends Omit<GoToPageActions, 'goToEditorPage'> {}

/** State machine data. */
interface ActionsStateData {
  readonly editorPage: EditorPageData
}

/** State machine actions. */
interface ActionsStateActions {
  readonly editorPage: EditorPageActions
}

registerAction('editorPage', (page, _data, promise) => ({
  ...goToPageActions(page, promise),
}))

// ====================
// === settingsPage ===
// ====================

/** Data for the "settings page" state. */
interface SettingsPageData {}

/** Actions for the "settings page" state. */
interface SettingsPageActions extends Omit<GoToPageActions, 'goToSettingsPage'> {}

/** State machine data. */
interface ActionsStateData {
  readonly settingsPage: SettingsPageData
}

/** State machine actions. */
interface ActionsStateActions {
  readonly settingsPage: SettingsPageActions
}

registerAction('settingsPage', (page, _data, promise) => ({
  ...goToPageActions(page, promise),
}))

// ========================
// === newDataLinkModal ===
// ========================

/** Locate the "new data link" modal. */
function locateNewDataLinkModal(page: test.Page) {
  return page.getByRole('heading').and(page.getByText('Create Data Link')).locator('..')
}

/** Data for the "new data link modal" state. */
interface NewDataLinkModalData {}

/** Actions for the "new data link modal" state. */
interface NewDataLinkModalActions extends Pick<GoToPageActions, 'goToHomePage'> {
  readonly withNameInput: (
    callback: (input: test.Locator) => Promise<void>
  ) => ActionsStateActionsForState<'newDataLinkModal'>
}

/** State machine data. */
interface ActionsStateData {
  readonly newDataLinkModal: NewDataLinkModalData
}

/** State machine actions. */
interface ActionsStateActions {
  readonly newDataLinkModal: NewDataLinkModalActions
}

/** Execute a {@link test.test.step} after the given {@link Promise} finishes. */
function thenStep(promise: Promise<void>, step: string, callback: () => Promise<void>) {
  return promise.then(() => test.test.step(step, callback))
}

registerAction('newDataLinkModal', (page, _data, promise) => ({
  goToHomePage: () =>
    actions(
      page,
      'homePage',
      {},
      thenStep(promise, 'Close "new data link" modal', () => press(page, 'Escape'))
    ),
  withNameInput: callback =>
    actions(
      page,
      'newDataLinkModal',
      {},
      thenStep(promise, 'Interact with "name" input', () =>
        callback(locateNewDataLinkModal(page).getByLabel('Name'))
      )
    ),
}))

// TODO: stop exporting all the actions - instead hide everything behind the state machine
// TODO: use unique symbols to prevent forgery. do not export the unique symbols so that
// it will be an error if `.finish()` is not called.

/** Create an object which can be used to navigate around a "new Data Link" modal. */
export function newDataLink(page: test.Page) {
  return {
    /** Find a "new Data Link" modal. */
    locate() {
      return page.getByRole('heading').and(page.getByText('Create Data Link')).locator('..')
    },
    /** Open a "new Data Link" modal. */
    go() {
      return this.locateOpenButton().click()
    },
    /** Find a "new Data Link" button. */
    locateOpenButton() {
      return page.getByRole('button').filter({ has: page.getByAltText('New Data Link') })
    },
    /** Find a "name" input. */
    locateNameInput() {
      return this.locate().getByLabel('Name')
    },
  }
}

export namespace settings {
  export namespace tab {
    export namespace organization {
      /** Find an "organization" tab button. */
      export function locate(page: test.Page) {
        return page.getByRole('button', { name: 'Organization' }).getByText('Organization')
      }
    }
    export namespace members {
      /** Find a "members" tab button. */
      export function locate(page: test.Page) {
        return page.getByRole('button', { name: 'Members', exact: true }).getByText('Members')
      }
    }
  }

  export namespace userAccount {
    /** Navigate so that the "user account" settings section is visible. */
    export async function go(page: test.Page) {
      await test.test.step('Go to "user account" settings section', async () => {
        await press(page, 'Mod+,')
      })
    }

    /** Find a "user account" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('User Account')).locator('..')
    }

    /** Find a "name" input in the "user account" settings section. */
    export function locateNameInput(page: test.Page) {
      return locate(page).getByLabel('Name')
    }
  }

  export namespace changePassword {
    /** Navigate so that the "change password" settings section is visible. */
    export async function go(page: test.Page) {
      await test.test.step('Go to "change password" settings section', async () => {
        await press(page, 'Mod+,')
      })
    }

    /** Find a "change password" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Change Password')).locator('..')
    }

    /** Find a "current password" input in the "user account" settings section. */
    export function locateCurrentPasswordInput(page: test.Page) {
      return locate(page).getByLabel('Current password')
    }

    /** Find a "new password" input in the "user account" settings section. */
    export function locateNewPasswordInput(page: test.Page) {
      return locate(page).getByLabel('New password', { exact: true })
    }

    /** Find a "confirm new password" input in the "user account" settings section. */
    export function locateConfirmNewPasswordInput(page: test.Page) {
      return locate(page).getByLabel('Confirm new password')
    }

    /** Find a "change" button. */
    export function locateChangeButton(page: test.Page) {
      return locate(page).getByRole('button', { name: 'Change' }).getByText('Change')
    }
  }

  export namespace profilePicture {
    /** Navigate so that the "profile picture" settings section is visible. */
    export async function go(page: test.Page) {
      await test.test.step('Go to "profile picture" settings section', async () => {
        await press(page, 'Mod+,')
      })
    }

    /** Find a "profile picture" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Profile Picture')).locator('..')
    }

    /** Find a "profile picture" input. */
    export function locateInput(page: test.Page) {
      return locate(page).locator('label')
    }
  }

  export namespace organization {
    /** Navigate so that the "organization" settings section is visible. */
    export async function go(page: test.Page) {
      await test.test.step('Go to "organization" settings section', async () => {
        await press(page, 'Mod+,')
        await settings.tab.organization.locate(page).click()
      })
    }

    /** Find an "organization" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Organization')).locator('..')
    }

    /** Find a "name" input in the "organization" settings section. */
    export function locateNameInput(page: test.Page) {
      return locate(page).getByLabel('Organization display name')
    }

    /** Find an "email" input in the "organization" settings section. */
    // eslint-disable-next-line @typescript-eslint/no-shadow
    export function locateEmailInput(page: test.Page) {
      return locate(page).getByLabel('Email')
    }

    /** Find an "website" input in the "organization" settings section. */
    export function locateWebsiteInput(page: test.Page) {
      return locate(page).getByLabel('Website')
    }

    /** Find an "location" input in the "organization" settings section. */
    export function locateLocationInput(page: test.Page) {
      return locate(page).getByLabel('Location')
    }
  }

  export namespace organizationProfilePicture {
    /** Navigate so that the "organization profile picture" settings section is visible. */
    export async function go(page: test.Page) {
      await test.test.step('Go to "organization profile picture" settings section', async () => {
        await press(page, 'Mod+,')
        await settings.tab.organization.locate(page).click()
      })
    }

    /** Find an "organization profile picture" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Profile Picture')).locator('..')
    }

    /** Find a "profile picture" input. */
    export function locateInput(page: test.Page) {
      return locate(page).locator('label')
    }
  }

  export namespace members {
    /** Navigate so that the "members" settings section is visible. */
    export async function go(page: test.Page, force = false) {
      await test.test.step('Go to "members" settings section', async () => {
        await press(page, 'Mod+,')
        await settings.tab.members.locate(page).click({ force })
      })
    }

    /** Find a "members" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Members')).locator('..')
    }

    /** Find all rows representing members of the current organization. */
    export function locateMembersRows(page: test.Page) {
      return locate(page).locator('tbody').getByRole('row')
    }
  }
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

/** A test assertion to confirm that the element has the class `selected`. */
export async function expectClassSelected(locator: test.Locator) {
  await test.test.step('Expect `selected`', async () => {
    await test.expect(locator).toHaveClass(/(?:^| )selected(?: |$)/)
  })
}

/** A test assertion to confirm that the element has the class `selected`. */
export async function expectNotTransparent(locator: test.Locator) {
  await test.test.step('expect.not.transparent', async () => {
    await test.expect
      .poll(() => locator.evaluate(element => getComputedStyle(element).opacity))
      .not.toBe('0')
  })
}

/** A test assertion to confirm that the element has the class `selected`. */
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

// eslint-disable-next-line @typescript-eslint/no-magic-numbers
const ASSET_ROW_SAFE_POSITION = { x: 300, y: 16 }

/** Click an asset row. The center must not be clicked as that is the button for adding a label. */
export async function clickAssetRow(assetRow: test.Locator) {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  await assetRow.click({ position: ASSET_ROW_SAFE_POSITION })
}

/** Drag an asset row. The center must not be clicked as that is the button for adding a label. */
export async function dragAssetRowToAssetRow(from: test.Locator, to: test.Locator) {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  await from.dragTo(to, {
    sourcePosition: ASSET_ROW_SAFE_POSITION,
    targetPosition: ASSET_ROW_SAFE_POSITION,
  })
}

/** Drag an asset row. The center must not be clicked as that is the button for adding a label. */
export async function dragAssetRow(from: test.Locator, to: test.Locator) {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  await from.dragTo(to, { sourcePosition: ASSET_ROW_SAFE_POSITION })
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
  await test.test.step(`Press '${keyOrShortcut}'`, async () => {
    if (/\bMod\b|\bDelete\b/.test(keyOrShortcut)) {
      let userAgent = ''
      await test.test.step('Detect browser OS', async () => {
        userAgent = await page.evaluate(() => navigator.userAgent)
      })
      const isMacOS = /\bMac OS\b/i.test(userAgent)
      const ctrlKey = isMacOS ? 'Meta' : 'Control'
      const deleteKey = isMacOS ? 'Backspace' : 'Delete'
      const shortcut = keyOrShortcut.replace(/\bMod\b/, ctrlKey).replace(/\bDelete\b/, deleteKey)
      await page.keyboard.press(shortcut)
    } else {
      await page.keyboard.press(keyOrShortcut)
    }
  })
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
  await test.test.step('Login', async () => {
    await page.goto('/')
    await locateEmailInput(page).fill(email)
    await locatePasswordInput(page).fill(password)
    await locateLoginButton(page).click()
    await locateToastCloseButton(page).click()
  })
}

// ==============================
// === mockIsInPlaywrightTest ===
// ==============================

/** Inject `isInPlaywrightTest` into the page. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockIsInPlaywrightTest({ page }: MockParams) {
  await test.test.step('Mock `isInPlaywrightTest`', async () => {
    await page.evaluate(() => {
      // @ts-expect-error This is SAFE - it is a mistake for this variable to be written to
      // from anywhere else.
      window.isInPlaywrightTest = true
    })
  })
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
  await test.test.step('Mock Date', async () => {
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
  })
}

// ========================
// === mockIDEContainer ===
// ========================

/** Make the IDE container have a non-zero size. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockIDEContainer({ page }: MockParams) {
  await test.test.step('Mock IDE container', async () => {
    await page.evaluate(() => {
      const ideContainer = document.getElementById('app')
      if (ideContainer) {
        ideContainer.style.height = '100vh'
        ideContainer.style.width = '100vw'
      }
    })
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
  return await test.test.step('Execute all mocks', async () => {
    const api = await mockApi({ page })
    await mockIsInPlaywrightTest({ page })
    await mockDate({ page })
    await mockIDEContainer({ page })
    return { api, pageActions: loggedOutActions(page) }
  })
}

// =======================
// === mockAllAndLogin ===
// =======================

/** Set up all mocks, and log in with dummy credentials. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockAllAndLogin({ page }: MockParams) {
  return await test.test.step('Execute all mocks and login', async () => {
    const mocks = await mockAll({ page })
    await login({ page })
    // This MUST run after login because the element's styles are reset when the browser
    // is navigated to another page.
    await mockIDEContainer({ page })
    // This MUST also run after login because globals are reset when the browser
    // is navigated to another page.
    await mockIsInPlaywrightTest({ page })
    return { ...mocks, pageActions: loggedInActions(page) }
  })
}
