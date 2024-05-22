/** @file Actions for the "drive" page. */
import * as test from 'playwright/test'

import * as actions from '../actions'
import type * as baseActions from './BaseActions'
import BaseActions from './BaseActions'
import * as contextMenuActions from './contextMenuActions'
import EditorPageActions from './EditorPageActions'
import * as goToPageActions from './goToPageActions'
import NewDataLinkModalActions from './NewDataLinkModalActions'
import * as openUserMenuAction from './openUserMenuAction'
import * as userMenuActions from './userMenuActions'

// ========================
// === DrivePageActions ===
// ========================

/** Actions for the "drive" page. */
export default class DrivePageActions extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, 'drive'> {
    return goToPageActions.goToPageActions(this.step.bind(this))
  }

  /** Actions related to the User Menu. */
  get userMenu() {
    return userMenuActions.userMenuActions(this.step.bind(this))
  }

  /** Actions related to context menus. */
  get contextMenu() {
    return contextMenuActions.contextMenuActions(this.step.bind(this))
  }

  /** Switch to a different category. */
  get goToCategory() {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self: DrivePageActions = this
    return {
      /** Switch to the "home" category. */
      home() {
        return self.step('Go to "Home" category', page =>
          page.getByRole('button', { name: 'Home' }).getByText('Home').click()
        )
      },
      /** Switch to the "recent" category. */
      recent() {
        return self.step('Go to "Recent" category', page =>
          page.getByRole('button', { name: 'Recent' }).getByText('Recent').click()
        )
      },
      /** Switch to the "trash" category. */
      trash() {
        return self.step('Go to "Trash" category', page =>
          page.getByRole('button', { name: 'Trash' }).getByText('Trash').click()
        )
      },
    }
  }

  /** Actions specific to the Drive table. */
  get driveTable() {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self: DrivePageActions = this
    return {
      /** Click the column heading for the "name" column to change its sort order. */
      clickNameColumnHeading() {
        return self.step('Click "name" column heading', page =>
          page.getByLabel('Sort by name').or(page.getByLabel('Stop sorting by name')).click()
        )
      },
      /** Click the column heading for the "modified" column to change its sort order. */
      clickModifiedColumnHeading() {
        return self.step('Click "modified" column heading', page =>
          page
            .getByLabel('Sort by modification date')
            .or(page.getByLabel('Stop sorting by modification date'))
            .click()
        )
      },
      /** Click to select a specific row. */
      clickRow(index: number) {
        return self.step('Click drive table row', page =>
          actions
            .locateAssetRows(page)
            .nth(index)
            .click({ position: actions.ASSET_ROW_SAFE_POSITION })
        )
      },
      /** Right click a specific row to bring up its context menu, or the context menu for multiple
       * assets when right clicking on a selected asset when multiple assets are selected. */
      rightClickRow(index: number) {
        return self.step('Click drive table row', page =>
          actions
            .locateAssetRows(page)
            .nth(index)
            .click({ button: 'right', position: actions.ASSET_ROW_SAFE_POSITION })
        )
      },
      /** Interact with the set of all rows in the Drive table. */
      withRows(callback: baseActions.LocatorCallback) {
        return self.step('Interact with drive table rows', async page => {
          await callback(actions.locateAssetRows(page))
        })
      },
      /** A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets to show. */
      expectPlaceholderRow() {
        return self.step('Expect placeholder row', async page => {
          const assetRows = actions.locateAssetRows(page)
          await test.expect(assetRows).toHaveCount(1)
          await test.expect(assetRows).toHaveText(/You have no files/)
        })
      },
      /** A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets in Trash. */
      expectTrashPlaceholderRow() {
        return self.step('Expect trash placeholder row', async page => {
          const assetRows = actions.locateAssetRows(page)
          await test.expect(assetRows).toHaveCount(1)
          await test.expect(assetRows).toHaveText(/Your trash is empty/)
        })
      },
    }
  }

  /** Open the User Menu. */
  openUserMenu() {
    return openUserMenuAction.openUserMenuAction(this.step.bind(this))
  }

  /** Interact with the drive view (the main container of this page). */
  withDriveView(callback: baseActions.LocatorCallback) {
    return this.step('Interact with drive view', page => callback(actions.locateDriveView(page)))
  }

  /** Create a new project using the button in the Drive Bar. */
  createProject() {
    return this.step('Create project', page =>
      page.getByRole('button', { name: 'New Project' }).getByText('New Project').click()
    ).into(EditorPageActions)
  }

  /** Create a new folder using the icon in the Drive Bar. */
  createFolder() {
    return this.step('Create folder', page =>
      page
        .getByRole('button')
        .filter({ has: page.getByAltText('New Folder') })
        .click()
    )
  }

  /** Upload a file using the icon in the Drive Bar. */
  uploadFile(
    name: string,
    contents: WithImplicitCoercion<Uint8Array | string | readonly number[]>,
    mimeType = 'text/plain'
  ) {
    return this.step(`Upload file '${name}'`, async page => {
      const fileChooserPromise = page.waitForEvent('filechooser')
      await page
        .getByRole('button')
        .filter({ has: page.getByAltText('Import') })
        .click()
      const fileChooser = await fileChooserPromise
      await fileChooser.setFiles([{ name, buffer: Buffer.from(contents), mimeType }])
    })
  }

  /** Create a new secret using the icon in the Drive Bar. */
  createSecret(name: string, value: string) {
    return this.step(`Create secret '${name}' = '${value}'`, async page => {
      await actions.locateNewSecretIcon(page).click()
      await actions.locateSecretNameInput(page).fill(name)
      await actions.locateSecretValueInput(page).fill(value)
      await actions.locateCreateButton(page).click()
    })
  }

  /** Toggle the Asset Panel open or closed. */
  toggleAssetPanel() {
    return this.step('Toggle asset panel', page =>
      page.getByAltText('Asset Panel').locator('visible=true').click()
    )
  }

  /** Interact with the Asset Panel. */
  withAssetPanel(callback: baseActions.LocatorCallback) {
    return this.step('Interact with asset panel', async page => {
      await callback(actions.locateAssetPanel(page))
    })
  }

  /** Open the Data Link creation modal by clicking on the Data Link icon. */
  openDataLinkModal() {
    return this.step('Open "new data link" modal', page =>
      page
        .getByRole('button')
        .filter({ has: page.getByAltText('New Data Link') })
        .click()
    ).into(NewDataLinkModalActions)
  }

  /** Interact with the context menus (the context menus MUST be visible). */
  withContextMenus(callback: baseActions.LocatorCallback) {
    return this.step('Interact with context menus', async page => {
      await callback(actions.locateContextMenus(page))
    })
  }
}
