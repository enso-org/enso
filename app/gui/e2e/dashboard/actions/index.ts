/** @file Various actions, locators, and constants used in end-to-end tests. */
import * as test from '@playwright/test'

import { TEXTS } from 'enso-common/src/text'

import * as apiModule from '../api'
import DrivePageActions from './DrivePageActions'
import LoginPageActions from './LoginPageActions'

// =================
// === Constants ===
// =================

/** An example password that does not meet validation requirements. */
export const INVALID_PASSWORD = 'password'
/** An example password that meets validation requirements. */
export const VALID_PASSWORD = 'Password0!'
/** An example valid email address. */
export const VALID_EMAIL = 'email@example.com'
export const TEXT = TEXTS.english

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

/** Find a "name" input for a "new label" modal (if any) on the current page. */
export function locateNewLabelModalNameInput(page: test.Page) {
  return locateNewLabelModal(page).getByLabel('Name').and(page.getByRole('textbox'))
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
  return locateUpsertSecretModal(page).getByPlaceholder(TEXT.secretNamePlaceholder)
}

/** Find a "value" input for an "upsert secret" modal (if any) on the current page. */
export function locateSecretValueInput(page: test.Page) {
  return locateUpsertSecretModal(page).getByPlaceholder(TEXT.secretValuePlaceholder)
}

/** Find a search bar input (if any) on the current page. */
export function locateSearchBarInput(page: test.Page) {
  return locateSearchBar(page).getByPlaceholder(/(?:)/)
}

/** Find the name column of the given assets table row. */
export function locateAssetRowName(locator: test.Locator) {
  return locator.getByTestId('asset-row-name')
}

// === Button locators ===

/** Find a "login" button (if any) on the current locator. */
export function locateLoginButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Login', exact: true }).getByText('Login')
}

/** Find a "register" button (if any) on the current locator. */
export function locateRegisterButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Register' }).getByText('Register')
}

/** Find a "create" button (if any) on the current page. */
export function locateCreateButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Create' }).getByText('Create')
}

/** Find a button to open the editor (if any) on the current page. */
export function locatePlayOrOpenProjectButton(page: test.Locator | test.Page) {
  return page.getByLabel('Open in editor')
}

/** Find a button to close the project (if any) on the current page. */
export function locateStopProjectButton(page: test.Locator | test.Page) {
  return page.getByLabel('Stop execution')
}

/** Close a modal. */
export function closeModal(page: test.Page) {
  return test.test.step('Close modal', async () => {
    await page.getByLabel('Close').click()
  })
}

/** Find all labels in the labels panel (if any) on the current page. */
export function locateLabelsPanelLabels(page: test.Page, name?: string) {
  return (
    locateLabelsPanel(page)
      .getByRole('button')
      .filter(name != null ? { has: page.getByText(name) } : {})
      // The delete button is also a `button`.
      .and(page.locator(':nth-child(1)'))
  )
}

/** Find a tick button (if any) on the current page. */
export function locateEditingTick(page: test.Locator | test.Page) {
  return page.getByLabel('Confirm Edit')
}

/** Find a cross button (if any) on the current page. */
export function locateEditingCross(page: test.Locator | test.Page) {
  return page.getByLabel('Cancel Edit')
}

/** Find labels in the "Labels" column of the assets table (if any) on the current page. */
export function locateAssetLabels(page: test.Locator | test.Page) {
  return page.getByTestId('asset-label')
}

/** Find a toggle for the "Name" column (if any) on the current page. */
export function locateNameColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Name')
}

/** Find a toggle for the "Modified" column (if any) on the current page. */
export function locateModifiedColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Modified')
}

/** Find a toggle for the "Shared with" column (if any) on the current page. */
export function locateSharedWithColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Shared With')
}

/** Find a toggle for the "Labels" column (if any) on the current page. */
export function locateLabelsColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Labels')
}

/** Find a toggle for the "Accessed by projects" column (if any) on the current page. */
export function locateAccessedByProjectsColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Accessed By Projects')
}

/** Find a toggle for the "Accessed data" column (if any) on the current page. */
export function locateAccessedDataColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Accessed Data')
}

/** Find a toggle for the "Docs" column (if any) on the current page. */
export function locateDocsColumnToggle(page: test.Locator | test.Page) {
  return page.getByLabel('Docs')
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

// === Other buttons ===

/** Find a "new label" button (if any) on the current page. */
export function locateNewLabelButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'new label' }).getByText('new label')
}

/** Find an "upgrade" button (if any) on the current page. */
export function locateUpgradeButton(page: test.Locator | test.Page) {
  return page.getByRole('link', { name: 'Upgrade', exact: true }).getByText('Upgrade').first()
}

/** Find a not enabled stub view (if any) on the current page. */
export function locateNotEnabledStub(page: test.Locator | test.Page) {
  return page.getByTestId('not-enabled-stub')
}

/** Find a "new folder" icon (if any) on the current page. */
export function locateNewFolderIcon(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Folder', exact: true })
}

/** Find a "new secret" icon (if any) on the current page. */
export function locateNewSecretIcon(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'New Secret' })
}

/** Find a "download files" icon (if any) on the current page. */
export function locateDownloadFilesIcon(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Export' })
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

// === Heading locators ===

/** Find a "name" column heading (if any) on the current page. */
export function locateNameColumnHeading(page: test.Locator | test.Page) {
  return page
    .getByLabel('Sort by name')
    .or(page.getByLabel('Stop sorting by name'))
    .or(page.getByLabel('Sort by name descending'))
}

/** Find a "modified" column heading (if any) on the current page. */
export function locateModifiedColumnHeading(page: test.Locator | test.Page) {
  return page
    .getByLabel('Sort by modification date')
    .or(page.getByLabel('Stop sorting by modification date'))
    .or(page.getByLabel('Sort by modification date descending'))
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

/** Find an editor container (if any) on the current page. */
export function locateEditor(page: test.Page) {
  // Test ID of a placeholder editor component used during testing.
  return page.locator('.App')
}

/** Find an assets table (if any) on the current page. */
export function locateAssetsTable(page: test.Page) {
  return locateDriveView(page).getByRole('table')
}

/** Find assets table rows (if any) on the current page. */
export function locateAssetRows(page: test.Page) {
  return locateAssetsTable(page).getByTestId('asset-row')
}

/** Find assets table placeholder rows (if any) on the current page. */
export function locateNonAssetRows(page: test.Page) {
  return locateAssetsTable(page).locator('tbody tr:not([data-testid="asset-row"])')
}

/** Find the name column of the given asset row. */
export function locateAssetName(locator: test.Locator) {
  return locator.locator('> :nth-child(1)')
}

/**
 * Find assets table rows that represent directories that can be expanded (if any)
 * on the current page.
 */
export function locateExpandableDirectories(page: test.Page) {
  // The icon is hidden when not hovered so `getByLabel` will not work.
  return locateAssetRows(page).filter({ has: page.locator('[aria-label=Expand]') })
}

/**
 * Find assets table rows that represent directories that can be collapsed (if any)
 * on the current page.
 */
export function locateCollapsibleDirectories(page: test.Page) {
  // The icon is hidden when not hovered so `getByLabel` will not work.
  return locateAssetRows(page).filter({ has: page.locator('[aria-label=Collapse]') })
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

/** Find a user menu (if any) on the current page. */
export function locateUserMenu(page: test.Page) {
  return page.getByLabel(TEXT.userMenuLabel).and(page.getByRole('button')).locator('visible=true')
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
  return page.getByTestId('asset-panel').locator('visible=true')
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

/**
 * Find a root directory dropzone (if any) on the current page.
 * This is the empty space below the assets table, if it doesn't take up the whole screen
 * vertically.
 */
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
      })
    }

    /** Find a "change password" settings section. */
    export function locate(page: test.Page) {
      return page.getByRole('heading').and(page.getByText('Change Password')).locator('..')
    }

    /** Find a "current password" input in the "user account" settings section. */
    export function locateCurrentPasswordInput(page: test.Page) {
      return locate(page).getByRole('group', { name: 'Current password' }).getByRole('textbox')
    }

    /** Find a "new password" input in the "user account" settings section. */
    export function locateNewPasswordInput(page: test.Page) {
      return locate(page)
        .getByRole('group', { name: /^New password/, exact: true })
        .getByRole('textbox')
    }

    /** Find a "confirm new password" input in the "user account" settings section. */
    export function locateConfirmNewPasswordInput(page: test.Page) {
      return locate(page)
        .getByRole('group', { name: /^Confirm new password/, exact: true })
        .getByRole('textbox')
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
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
        await locateUserMenu(page).click()
        await page.getByRole('button', { name: 'Settings' }).getByText('Settings').click()
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

/**
 * Get the left side of the bounding box of an asset row. The locator MUST be for an asset row.
 * DO NOT assume the left side of the outer container will change. This means that it is NOT SAFE
 * to do anything with the returned values other than comparing them.
 */
export function getAssetRowLeftPx(locator: test.Locator) {
  return locator.evaluate((el) => el.children[0]?.children[0]?.getBoundingClientRect().left ?? 0)
}

// ===================================
// === Expect functions for themes ===
// ===================================

/** A test assertion to confirm that the element has the class `selected`. */
export async function expectClassSelected(locator: test.Locator) {
  await test.test.step('Expect `selected`', async () => {
    await test.expect(locator).toHaveClass(/(?:^| )selected(?: |$)/)
  })
}

// ==============================
// === Other expect functions ===
// ==============================

/** A test assertion to confirm that the element is fully transparent. */
export async function expectOpacity0(locator: test.Locator) {
  await test.test.step('Expect `opacity: 0`', async () => {
    await test
      .expect(async () => {
        test.expect(await locator.evaluate((el) => getComputedStyle(el).opacity)).toBe('0')
      })
      .toPass()
  })
}

/** A test assertion to confirm that the element is not fully transparent. */
export async function expectNotOpacity0(locator: test.Locator) {
  await test.test.step('Expect not `opacity: 0`', async () => {
    await test
      .expect(async () => {
        test.expect(await locator.evaluate((el) => getComputedStyle(el).opacity)).not.toBe('0')
      })
      .toPass()
  })
}

/** A test assertion to confirm that the element is onscreen. */
export async function expectOnScreen(locator: test.Locator) {
  await test.test.step('Expect to be onscreen', async () => {
    await test
      .expect(async () => {
        const pageBounds = await locator.evaluate(() => document.body.getBoundingClientRect())
        const bounds = await locator.evaluate((el) => el.getBoundingClientRect())
        test
          .expect(
            bounds.left < pageBounds.right &&
              bounds.right > pageBounds.left &&
              bounds.top < pageBounds.bottom &&
              bounds.bottom > pageBounds.top,
          )
          .toBe(true)
      })
      .toPass()
  })
}

/** A test assertion to confirm that the element is onscreen. */
export async function expectNotOnScreen(locator: test.Locator) {
  await test.test.step('Expect to not be onscreen', async () => {
    await test
      .expect(async () => {
        const pageBounds = await locator.evaluate(() => document.body.getBoundingClientRect())
        const bounds = await locator.evaluate((el) => el.getBoundingClientRect())
        test
          .expect(
            bounds.left >= pageBounds.right ||
              bounds.right <= pageBounds.left ||
              bounds.top >= pageBounds.bottom ||
              bounds.bottom <= pageBounds.top,
          )
          .toBe(true)
      })
      .toPass()
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

/**
 * Press a key, replacing the text `Mod` with `Meta` (`Cmd`) on macOS, and `Control`
 * on all other platforms.
 */
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

// ===============================
// === Miscellaneous utilities ===
// ===============================

/** Perform a successful login. */
export async function login(
  { page, setupAPI }: MockParams,
  email = 'email@example.com',
  password = VALID_PASSWORD,
  first = true,
) {
  await test.test.step('Login', async () => {
    await locateEmailInput(page).fill(email)
    await locatePasswordInput(page).fill(password)
    await locateLoginButton(page).click()
    await test.expect(page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
    if (first) {
      await passAgreementsDialog({ page, setupAPI })
      await test.expect(page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
    }
  })
}

/** Reload. */
export async function reload({ page }: MockParams) {
  await test.test.step('Reload', async () => {
    await page.reload()
    await test.expect(page.getByText(TEXT.loadingAppMessage)).not.toBeVisible()
  })
}

/** Logout and then login again. */
export async function relog(
  { page, setupAPI }: MockParams,
  email = 'email@example.com',
  password = VALID_PASSWORD,
) {
  await test.test.step('Relog', async () => {
    await page.getByLabel(TEXT.userMenuLabel).locator('visible=true').click()
    await page
      .getByRole('button', { name: TEXT.signOutShortcut })
      .getByText(TEXT.signOutShortcut)
      .click()
    await login({ page, setupAPI }, email, password, false)
  })
}

/** A placeholder date for visual regression testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45'))

/** Parameters for {@link mockDate}. */
interface MockParams {
  readonly page: test.Page
  readonly setupAPI?: apiModule.SetupAPI | undefined
}

/** Replace `Date` with a version that returns a fixed time. */
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

/** Pass the Agreements dialog. */
export async function passAgreementsDialog({ page }: MockParams) {
  await test.test.step('Accept Terms and Conditions', async () => {
    await page.waitForSelector('#agreements-modal')
    await page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox)
      .click()
    await page
      .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
      .getByText(TEXT.privacyPolicyCheckbox)
      .click()
    await page.getByRole('button', { name: 'Accept' }).click()
  })
}

export const mockApi = apiModule.mockApi

/** Set up all mocks, without logging in. */
export function mockAll({ page, setupAPI }: MockParams) {
  return new LoginPageActions(page).step('Execute all mocks', async () => {
    await mockApi({ page, setupAPI })
    await mockDate({ page, setupAPI })
    await page.goto('/')
  })
}

/** Set up all mocks, and log in with dummy credentials. */
export function mockAllAndLogin({ page, setupAPI }: MockParams) {
  return new DrivePageActions(page)
    .step('Execute all mocks', async () => {
      await mockApi({ page, setupAPI })
      await mockDate({ page, setupAPI })
      await page.goto('/')
    })
    .do((thePage) => login({ page: thePage, setupAPI }))
}

/**
 * Set up all mocks, and log in with dummy credentials.
 * @deprecated Prefer {@link mockAllAndLogin}.
 */
export async function mockAllAndLoginAndExposeAPI({ page, setupAPI }: MockParams) {
  return await test.test.step('Execute all mocks and login', async () => {
    const api = await mockApi({ page, setupAPI })
    await mockDate({ page, setupAPI })
    await page.goto('/')
    await login({ page, setupAPI })
    return api
  })
}
