/** @file Actions for the "drive" page. */
import * as test from 'playwright/test'

import {
  locateAssetPanel,
  locateAssetsTable,
  locateContextMenus,
  locateCreateButton,
  locateDriveView,
  locateNewSecretIcon,
  locateNonAssetRows,
  locateSecretNameInput,
  locateSecretValueInput,
  TEXT,
} from '.'
import type * as baseActions from './BaseActions'
import * as contextMenuActions from './contextMenuActions'
import EditorPageActions from './EditorPageActions'
import * as goToPageActions from './goToPageActions'
import NewDataLinkModalActions from './NewDataLinkModalActions'
import PageActions from './PageActions'
import StartModalActions from './StartModalActions'

// =================
// === Constants ===
// =================

const ASSET_ROW_SAFE_POSITION = { x: 300, y: 16 }

// =======================
// === locateAssetRows ===
// =======================

/** Find all assets table rows (if any). */
function locateAssetRows(page: test.Page) {
  return locateAssetsTable(page).getByTestId('asset-row')
}

// ========================
// === DrivePageActions ===
// ========================

/** Actions for the "drive" page. */
export default class DrivePageActions extends PageActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, 'drive'> {
    return goToPageActions.goToPageActions(this.step.bind(this))
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
      /** Switch to the "cloud" category. */
      cloud() {
        return self.step('Go to "Cloud" category', (page) =>
          page
            .getByRole('button', { name: TEXT.cloudCategory, exact: true })
            .getByText(TEXT.cloudCategory)
            .click(),
        )
      },
      /** Switch to the "local" category. */
      local() {
        return self.step('Go to "Local" category', (page) =>
          page
            .getByRole('button', { name: TEXT.localCategory, exact: true })
            .getByText(TEXT.localCategory)
            .click(),
        )
      },
      /** Switch to the "recent" category. */
      recent() {
        return self.step('Go to "Recent" category', (page) =>
          page
            .getByRole('button', { name: TEXT.recentCategory, exact: true })
            .getByText(TEXT.recentCategory)
            .click(),
        )
      },
      /** Switch to the "trash" category. */
      trash() {
        return self.step('Go to "Trash" category', (page) =>
          page.getByRole('button', { name: TEXT.trashCategory, exact: true }).click(),
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
        return self.step('Click "name" column heading', (page) =>
          page.getByLabel(TEXT.sortByName).or(page.getByLabel(TEXT.stopSortingByName)).click(),
        )
      },
      /** Click the column heading for the "modified" column to change its sort order. */
      clickModifiedColumnHeading() {
        return self.step('Click "modified" column heading', (page) =>
          page
            .getByLabel(TEXT.sortByModificationDate)
            .or(page.getByLabel(TEXT.stopSortingByModificationDate))
            .click(),
        )
      },
      /** Click to select a specific row. */
      clickRow(index: number) {
        return self.step(`Click drive table row #${index}`, (page) =>
          locateAssetRows(page).nth(index).click({ position: ASSET_ROW_SAFE_POSITION }),
        )
      },
      /**
       * Right click a specific row to bring up its context menu, or the context menu for multiple
       * assets when right clicking on a selected asset when multiple assets are selected.
       */
      rightClickRow(index: number) {
        return self.step(`Right click drive table row #${index}`, (page) =>
          locateAssetRows(page)
            .nth(index)
            .click({ button: 'right', position: ASSET_ROW_SAFE_POSITION }),
        )
      },
      /** Double click a row. */
      doubleClickRow(index: number) {
        return self.step(`Double dlick drive table row #${index}`, (page) =>
          locateAssetRows(page).nth(index).dblclick({ position: ASSET_ROW_SAFE_POSITION }),
        )
      },
      /** Interact with the set of all rows in the Drive table. */
      withRows(
        callback: (assetRows: test.Locator, nonAssetRows: test.Locator) => Promise<void> | void,
      ) {
        return self.step('Interact with drive table rows', async (page) => {
          await callback(locateAssetRows(page), locateNonAssetRows(page))
        })
      },
      /** Drag a row onto another row. */
      dragRowToRow(from: number, to: number) {
        return self.step(`Drag drive table row #${from} to row #${to}`, async (page) => {
          const rows = locateAssetRows(page)
          await rows.nth(from).dragTo(rows.nth(to), {
            sourcePosition: ASSET_ROW_SAFE_POSITION,
            targetPosition: ASSET_ROW_SAFE_POSITION,
          })
        })
      },
      /** Drag a row onto another row. */
      dragRow(from: number, to: test.Locator, force?: boolean) {
        return self.step(`Drag drive table row #${from} to custom locator`, (page) =>
          locateAssetRows(page)
            .nth(from)
            .dragTo(to, {
              sourcePosition: ASSET_ROW_SAFE_POSITION,
              ...(force == null ? {} : { force }),
            }),
        )
      },
      /**
       * A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets to show.
       */
      expectPlaceholderRow() {
        return self.step('Expect placeholder row', async (page) => {
          await test.expect(locateAssetRows(page)).toHaveCount(0)
          const nonAssetRows = locateNonAssetRows(page)
          await test.expect(nonAssetRows).toHaveCount(1)
          await test.expect(nonAssetRows).toHaveText(/This folder is empty/)
        })
      },
      /**
       * A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets in Trash.
       */
      expectTrashPlaceholderRow() {
        return self.step('Expect trash placeholder row', async (page) => {
          await test.expect(locateAssetRows(page)).toHaveCount(0)
          const nonAssetRows = locateNonAssetRows(page)
          await test.expect(nonAssetRows).toHaveCount(1)
          await test.expect(nonAssetRows).toHaveText(/Your trash is empty/)
        })
      },
      /** Toggle a column's visibility. */
      get toggleColumn() {
        return {
          /** Toggle visibility for the "modified" column. */
          modified() {
            return self.step('Toggle "modified" column', (page) =>
              page.getByLabel(TEXT.modifiedColumnName).click(),
            )
          },
          /** Toggle visibility for the "shared with" column. */
          sharedWith() {
            return self.step('Toggle "shared with" column', (page) =>
              page.getByLabel(TEXT.sharedWithColumnName).click(),
            )
          },
          /** Toggle visibility for the "labels" column. */
          labels() {
            return self.step('Toggle "labels" column', (page) =>
              page.getByLabel(TEXT.labelsColumnName).click(),
            )
          },
          /** Toggle visibility for the "accessed by projects" column. */
          accessedByProjects() {
            return self.step('Toggle "accessed by projects" column', (page) =>
              page.getByLabel(TEXT.accessedByProjectsColumnName).click(),
            )
          },
          /** Toggle visibility for the "accessed data" column. */
          accessedData() {
            return self.step('Toggle "accessed data" column', (page) =>
              page.getByLabel(TEXT.accessedDataColumnName).click(),
            )
          },
          /** Toggle visibility for the "docs" column. */
          docs() {
            return self.step('Toggle "docs" column', (page) =>
              page.getByLabel(TEXT.docsColumnName).click(),
            )
          },
        }
      },
    }
  }

  /** Open the "start" modal. */
  openStartModal() {
    return this.step('Open "start" modal', (page) =>
      page.getByText(TEXT.startWithATemplate).click(),
    ).into(StartModalActions)
  }

  /** Create a new empty project. */
  newEmptyProject() {
    return this.step('Create empty project', (page) =>
      page.getByText(TEXT.newEmptyProject, { exact: true }).click(),
    ).into(EditorPageActions)
  }

  /** Interact with the drive view (the main container of this page). */
  withDriveView(callback: baseActions.LocatorCallback) {
    return this.step('Interact with drive view', (page) => callback(locateDriveView(page)))
  }

  /** Create a new folder using the icon in the Drive Bar. */
  createFolder() {
    return this.step('Create folder', async (page) => {
      await page.getByRole('button', { name: TEXT.newFolder, exact: true }).click()
      await test.expect(page.locator('input:focus')).toBeVisible()
      await page.keyboard.press('Escape')
    })
  }

  /** Upload a file using the icon in the Drive Bar. */
  uploadFile(
    name: string,
    contents: WithImplicitCoercion<Uint8Array | string | readonly number[]>,
    mimeType = 'text/plain',
  ) {
    return this.step(`Upload file '${name}'`, async (page) => {
      const fileChooserPromise = page.waitForEvent('filechooser')
      await page.getByRole('button', { name: TEXT.uploadFiles }).click()
      const fileChooser = await fileChooserPromise
      await fileChooser.setFiles([{ name, buffer: Buffer.from(contents), mimeType }])
    })
  }

  /** Create a new secret using the icon in the Drive Bar. */
  createSecret(name: string, value: string) {
    return this.step(`Create secret '${name}' = '${value}'`, async (page) => {
      await locateNewSecretIcon(page).click()
      await locateSecretNameInput(page).fill(name)
      await locateSecretValueInput(page).fill(value)
      await locateCreateButton(page).click()
    })
  }

  /** Toggle the Asset Panel open or closed. */
  toggleAssetPanel() {
    return this.step('Toggle asset panel', (page) =>
      page.getByLabel('Asset Panel').locator('visible=true').click(),
    )
  }

  /** Interact with the container element of the assets table. */
  withAssetsTable(callback: baseActions.LocatorCallback) {
    return this.step('Interact with drive table', async (page) => {
      await callback(locateAssetsTable(page))
    })
  }

  /** Interact with the Asset Panel. */
  withAssetPanel(callback: baseActions.LocatorCallback) {
    return this.step('Interact with asset panel', async (page) => {
      await callback(locateAssetPanel(page))
    })
  }

  /** Open the Data Link creation modal by clicking on the Data Link icon. */
  openDataLinkModal() {
    return this.step('Open "new data link" modal', (page) =>
      page.getByRole('button', { name: TEXT.newDatalink }).click(),
    ).into(NewDataLinkModalActions)
  }

  /** Interact with the context menus (the context menus MUST be visible). */
  withContextMenus(callback: baseActions.LocatorCallback) {
    return this.step('Interact with context menus', async (page) => {
      await callback(locateContextMenus(page))
    })
  }
}
