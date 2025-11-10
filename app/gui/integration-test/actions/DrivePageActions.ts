/** @file Actions for the "drive" page. */
import { expect, type Locator, type Page } from 'integration-test/base'
import type { LocatorCallback } from './BaseActions'
import { contextMenuActions } from './contextMenuActions'
import EditorPageActions from './EditorPageActions'
import { goToPageActions, type GoToPageActions } from './goToPageActions'
import NewDataLinkModalActions from './NewDataLinkModalActions'
import PageActions from './PageActions'
import { TEXT } from './utilities'

const ASSET_ROW_SAFE_POSITION = { x: 150, y: 16 }

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

/** Find a drive view. .*/
function locateDriveView(page: Page) {
  // This has no identifying features.
  return page.getByTestId('drive-view')
}

/** Find a "create" button. */
function locateCreateButton(page: Page) {
  return page.getByRole('button', { name: TEXT.create }).getByText(TEXT.create)
}

/** Find an assets table. */
function locateAssetsTable(page: Page) {
  return page.getByTestId('drive-view').getByRole('table')
}

/** Find all assets table rows. */
function locateAssetRows(page: Page) {
  return locateAssetsTable(page).getByTestId('asset-row')
}

/** Find assets table placeholder rows. */
function locateNonAssetRows(page: Page) {
  return locateAssetsTable(page).locator(
    'tbody tr:not([data-testid="asset-row"]):not([data-testid="dummy-row"])',
  )
}

/** Find a "new secret" icon. */
function locateNewSecretIcon(page: Page) {
  return page.getByRole('button', { name: 'New Secret' })
}

/** Find an "upsert secret" modal. */
function locateUpsertSecretModal(page: Page) {
  // This has no identifying features.
  return page.getByTestId('upsert-secret-modal')
}

/** Find a "name" input for an "upsert secret" modal. */
function locateSecretNameInput(page: Page) {
  return locateUpsertSecretModal(page).getByPlaceholder(TEXT.secretNamePlaceholder)
}

/** Find a "value" input for an "upsert secret" modal. */
function locateSecretValueInput(page: Page) {
  return locateUpsertSecretModal(page).getByPlaceholder(TEXT.secretValuePlaceholder)
}

/** Find a radio button that navigates to specified drive category. */
function locateCategoryButton(page: Page, category: string): Locator {
  return page
    .getByLabel(TEXT.categorySwitcherMenuLabel)
    .getByRole('button', { name: category, exact: true })
}

/** Find an asset panel. */
function locateRightPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('right-panel').locator('visible=true')
}

/** Actions for the "drive" page. */
export default class DrivePageActions<Context = object> extends PageActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<GoToPageActions<Context>, 'drive'> {
    return goToPageActions(this.step.bind(this))
  }

  /** Actions related to context menus. */
  get contextMenu() {
    return contextMenuActions(this.step.bind(this))
  }

  /** Switch to a different category. */
  goToCategory = {
    /** Switch to the "cloud" category. */
    cloud: () => this.goToCategoryNamed(TEXT.cloudCategory),
    local: () => this.goToCategoryNamed(TEXT.localCategory),
    recent: () => this.goToCategoryNamed(TEXT.recentCategory),
    trash: () => this.goToCategoryNamed(TEXT.trashCategory),
  }

  /** Interact with the assets search bar. */
  withSearchBar(callback: LocatorCallback<Context>) {
    return this.step('Interact with search bar', (page, context) =>
      callback(page.getByTestId('asset-search-bar').getByPlaceholder(/(?:)/), context),
    )
  }

  /** Select category of specified name. */
  goToCategoryNamed(this: DrivePageActions<Context>, category: string) {
    return this.step(`Go to "${category}" category`, async (page) => {
      await locateCategoryButton(page, category).click()
      await this.expectCategory(category)
    })
  }

  /** Expect the category to be selected. */
  expectCategory(category: string) {
    return this.step(`Expect category '${category}'`, (page) =>
      expect(locateCategoryButton(page, category)).toHaveAttribute('data-selected', 'true'),
    )
  }

  /** Actions specific to the Drive table. */
  get driveTable() {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self: DrivePageActions<Context> = this
    const locateNameColumnHeading = (page: Page) =>
      page
        .getByLabel(TEXT.sortByName)
        .or(page.getByLabel(TEXT.sortByNameDescending))
        .or(page.getByLabel(TEXT.stopSortingByName))
    const locateModifiedColumnHeading = (page: Page) =>
      page
        .getByLabel(TEXT.sortByModificationDate)
        .or(page.getByLabel(TEXT.sortByModificationDateDescending))
        .or(page.getByLabel(TEXT.stopSortingByModificationDate))

    const locatePathColumnHeading = (page: Page) => page.getByTestId('path-column-heading')
    const locatePathColumnCell = (page: Page, title: string) =>
      page.getByTestId(`path-column-cell-${title.toLowerCase().replace(/\s+/g, '-')}`)

    const getRow = (page: Page, indexOrName: number | string) => {
      const rows = locateAssetRows(page)
      return typeof indexOrName === 'number' ?
          rows.nth(indexOrName)
        : rows.filter({ hasText: indexOrName })
    }

    return {
      /** Click the column heading for the "name" column to change its sort order. */
      clickNameColumnHeading() {
        return self.step('Click "name" column heading', (page) =>
          locateNameColumnHeading(page).click(),
        )
      },
      /** Interact with the column heading for the "name" column. */
      withNameColumnHeading(callback: LocatorCallback<Context>) {
        return self.step('Interact with "name" column heading', (page, context) =>
          callback(locateNameColumnHeading(page), context),
        )
      },
      withPathColumnHeading(callback: LocatorCallback<Context>) {
        return self.step('Interact with "path" column heading', (page, context) =>
          callback(locatePathColumnHeading(page), context),
        )
      },
      withPathColumnCell(title: string, callback: LocatorCallback<Context>) {
        return self.step(`Interact with "path" column of asset '${title}'`, (page, context) =>
          callback(locatePathColumnCell(page, title), context),
        )
      },
      /** Click the column heading for the "modified" column to change its sort order. */
      clickModifiedColumnHeading() {
        return self.step('Click "modified" column heading', (page) =>
          locateModifiedColumnHeading(page).click(),
        )
      },
      /** Interact with the column heading for the "modified" column. */
      withModifiedColumnHeading(callback: LocatorCallback<Context>) {
        return self.step('Interact with "modified" column heading', (page, context) =>
          callback(locateModifiedColumnHeading(page), context),
        )
      },
      /** Click to select a specific row. */
      clickRow(row: number | string) {
        return self.step(`Click drive table row '${row}'`, async (page) => {
          await getRow(page, row).click({ position: ASSET_ROW_SAFE_POSITION })
        })
      },
      /** Click the background to deselect all rows. */
      clickAway() {
        return self.step('Click drive table background', async (page) => {
          await page.getByTestId('assets-table-assets-unselector').first().click()
        })
      },
      /**
       * Right click a specific row to bring up its context menu, or the context menu for multiple
       * assets when right clicking on a selected asset when multiple assets are selected.
       */
      rightClickRow(row: number | string) {
        return self.step(`Right click drive table row '${row}'`, async (page) => {
          await getRow(page, row).click({ button: 'right', position: ASSET_ROW_SAFE_POSITION })
        })
      },
      /** Double click a row. */
      doubleClickRow(row: number | string) {
        return self.step(`Double dlick drive table row '${row}'`, async (page) => {
          await getRow(page, row).dblclick({ position: ASSET_ROW_SAFE_POSITION })
        })
      },
      /** Interact with the set of all rows in the Drive table. */
      withRows(
        callback: (
          assetRows: Locator,
          nonAssetRows: Locator,
          context: Context,
          page: Page,
        ) => Promise<void> | void,
      ) {
        return self.step('Interact with drive table rows', async (page) => {
          await callback(locateAssetRows(page), locateNonAssetRows(page), self.context, page)
        })
      },
      withSelectedRows(callback: LocatorCallback<Context>) {
        return self.step('Interact with selected drive table rows', async (page, context) => {
          await callback(locateAssetRows(page).and(page.locator('[data-selected="true"]')), context)
        })
      },
      /** Drag a row onto another row. */
      dragRowToRow(from: number | string, to: number | string) {
        return self.step(`Drag drive table row '${from}' to row '${to}'`, async (page) => {
          const fromRow = getRow(page, from)
          await fromRow.click()
          await fromRow.dragTo(getRow(page, to), {
            sourcePosition: ASSET_ROW_SAFE_POSITION,
            targetPosition: ASSET_ROW_SAFE_POSITION,
          })
        })
      },
      /** Drag a row onto another row. */
      dragRow(from: number | string, to: Locator, force = false) {
        return self.step(`Drag drive table row '${from}' to custom locator`, async (page) => {
          await getRow(page, from).dragTo(to, { sourcePosition: ASSET_ROW_SAFE_POSITION, force })
        })
      },
      /** Drag a row to a different category. */
      dragRowToCategory(row: number | string, category: 'Cloud' | 'Local' | 'Recent' | 'Trash') {
        return self.step(
          `Drag drive table row '${row}' to '${category}' category`,
          async (page) => {
            const categoryId = (
              {
                Cloud: 'cloudCategory',
                Local: 'localCategory',
                Recent: 'recentCategory',
                Trash: 'trashCategory',
              } satisfies { [C in typeof category]: `${Lowercase<C>}Category` & keyof typeof TEXT }
            )[category]

            const categoryElement = page
              .getByLabel(TEXT.categorySwitcherMenuLabel)
              .getByRole('button', { name: TEXT[categoryId], exact: true })
              .getByText(TEXT[categoryId])

            await getRow(page, row).dragTo(categoryElement, {
              sourcePosition: ASSET_ROW_SAFE_POSITION,
            })
          },
        )
      },
      /** Open a directory at a specific row. */
      openDirectory(row: number | string) {
        return self.step(`Open directory on drive table row ${row}`, async (page) => {
          const navigateButton = getRow(page, row).getByTestId('directory-row-navigate-button')
          await expect(navigateButton).toHaveAttribute('aria-label', TEXT.open)
          await navigateButton.click()
          await page.mouse.move(0, 0) // prevent popup from appearing
        })
      },
      /** Open a project at a specific row. */
      openProject(row: number | string) {
        return self.step(`Open directory on drive table row ${row}`, async (page) => {
          const button = getRow(page, row).getByLabel(TEXT.openInEditor)
          await button.click()
          await page.mouse.move(0, 0) // prevent popup from appearing
        })
      },
      /** Close a project at a specific row. */
      closeProject(row: number | string) {
        return self.step(`Open directory on drive table row ${row}`, async (page) => {
          const button = getRow(page, row).getByLabel(TEXT.stopExecution)
          await button.click()
          await page.mouse.move(0, 0) // prevent popup from appearing
        })
      },
      /**
       * A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets to show.
       */
      expectPlaceholderRow() {
        return self.step('Expect placeholder row', async (page) => {
          await expect(locateAssetRows(page)).toHaveCount(0)
          const nonAssetRows = locateNonAssetRows(page)
          await expect(nonAssetRows).toHaveCount(1)
          await expect(nonAssetRows).toHaveText(/This folder is empty/)
        })
      },
      /**
       * A test assertion to confirm that there is only one row visible, and that row is the
       * placeholder row displayed when there are no assets in Trash.
       */
      expectTrashPlaceholderRow() {
        return self.step('Expect trash placeholder row', async (page) => {
          await expect(locateAssetRows(page)).toHaveCount(0)
          const nonAssetRows = locateNonAssetRows(page)
          await expect(nonAssetRows).toHaveCount(1)
          await expect(nonAssetRows).toHaveText(/Your trash is empty/)
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
        }
      },
    }
  }

  /** Expect editor is opened due to external causes (like auto opening Welcome project). */
  expectProjectEditorOpened(name: string) {
    return this.step('Expect Editor is opened', async (page) => {
      const projectTab = page.getByRole('tab', { name })
      await expect(projectTab).toBeVisible({ timeout: 100000 })
      await expect(projectTab).toHaveClass(/selected/)
    }).into(EditorPageActions<Context>)
  }

  /** Clear trash. */
  clearTrash() {
    return this.step('Clear trash', async (page) => {
      await page.getByText(TEXT.clearTrash).click()
      await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
    })
  }

  /** Create a new empty project. */
  newEmptyProject() {
    return this.step('Create empty project', async (page) => {
      await page.getByText(TEXT.newEmptyProject, { exact: true }).click()
      await expect(page.locator('.ProjectView')).toBeVisible()
    }).into(EditorPageActions<Context>)
  }

  /** Interact with the drive view (the main container of this page). */
  withDriveView(callback: LocatorCallback<Context>) {
    return this.step('Interact with drive view', (page, context) =>
      callback(locateDriveView(page), context),
    )
  }

  /** Create a new folder using the icon in the Drive Bar. */
  createFolder() {
    return this.step('Create folder', async (page) => {
      await page.getByRole('button', { name: TEXT.newFolder, exact: true }).click()
      await expect(page.locator('input:focus')).toBeVisible()
      await page.keyboard.press('Escape')
    })
  }

  /** Upload a file using the icon in the Drive Bar. */
  uploadFile(
    name: string,
    contents: Uint8Array | string | readonly number[],
    mimeType = 'text/plain',
  ) {
    return this.step(`Upload file '${name}'`, async (page) => {
      const fileChooserPromise = page.waitForEvent('filechooser')
      await page.getByRole('button', { name: TEXT.uploadFiles, exact: true }).click()
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

  /** Show the properties tab of the Asset Panel. */
  togglePropertiesAssetPanel() {
    return this.step('Toggle properties asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Properties' }).click()
    })
  }

  /** Show the description tab of the Asset Panel. */
  toggleDescriptionAssetPanel() {
    return this.step('Toggle description asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Description' }).click()
    })
  }

  /** Show the Docs tab of the Asset Panel. */
  toggleDocsAssetPanel() {
    return this.step('Toggle docs asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Documentation' }).click()
    })
  }

  /** Interact with the container element of the assets table. */
  withAssetsTable(
    callback: (input: Locator, context: Context, page: Page) => Promise<void> | void,
  ) {
    return this.step('Interact with drive table', async (page) => {
      await callback(locateAssetsTable(page), this.context, page)
    })
  }

  /** Interact with the Asset Panel. */
  withRightPanel(callback: LocatorCallback<Context>) {
    return this.step('Interact with right panel', async (page, context) => {
      await callback(locateRightPanel(page), context)
    })
  }

  /** Open the Data Link creation modal by clicking on the Data Link icon. */
  openDataLinkModal() {
    return this.step('Open "new data link" modal', (page) =>
      page.getByRole('button', { name: TEXT.newDatalink }).click(),
    ).into(NewDataLinkModalActions<Context>)
  }

  /** Interact with the context menus (the context menus MUST be visible). */
  withContextMenus(callback: LocatorCallback<Context>) {
    return this.step('Interact with context menus', async (page, context) => {
      await callback(locateContextMenu(page), context)
    })
  }
}
