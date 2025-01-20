/** @file Actions for the "drive" page. */
import { expect, type Locator, type Page } from '@playwright/test'

import { TEXT } from '.'
import type { LocatorCallback } from './BaseActions'
import { contextMenuActions } from './contextMenuActions'
import EditorPageActions from './EditorPageActions'
import { goToPageActions, type GoToPageActions } from './goToPageActions'
import NewDataLinkModalActions from './NewDataLinkModalActions'
import PageActions from './PageActions'
import StartModalActions from './StartModalActions'

const ASSET_ROW_SAFE_POSITION = { x: 300, y: 16 }

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

/** Find a drive view. */
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

/** Find an asset panel. */
function locateAssetPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('asset-panel').locator('visible=true')
}

/** Actions for the "drive" page. */
export default class DrivePageActions<Context> extends PageActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<GoToPageActions<Context>, 'drive'> {
    return goToPageActions(this.step.bind(this))
  }

  /** Actions related to context menus. */
  get contextMenu() {
    return contextMenuActions(this.step.bind(this))
  }

  /** Switch to a different category. */
  get goToCategory() {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    const self: DrivePageActions<Context> = this
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

  /** Interact with the assets search bar. */
  withSearchBar(callback: LocatorCallback<Context>) {
    return this.step('Interact with search bar', (page, context) =>
      callback(page.getByTestId('asset-search-bar').getByPlaceholder(/(?:)/), context),
    )
  }

  /**
   * Expect the category to be selected.
   */
  expectCategory(category: string) {
    return this.step(`Expect category '${category}'`, (page) =>
      expect(page.getByRole('button', { name: category })).toHaveAttribute('data-selected', 'true'),
    )
  }

  /**
   * Expect the category to be not selected.
   */
  expectCategoryNotSelected(category: string) {
    return this.step(`Expect category '${category}' not selected`, (page) =>
      expect(page.getByRole('button', { name: category })).toHaveAttribute(
        'data-selected',
        'false',
      ),
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
        return self.step(`Interact with "path" column cell '${title}'`, (page, context) =>
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
      dragRowToRow(from: number, to: number) {
        return self.step(`Drag drive table row #${from} to row #${to}`, async (page) => {
          const rows = locateAssetRows(page)
          rows.nth(from).click()
          await rows.nth(from).dragTo(rows.nth(to), {
            sourcePosition: ASSET_ROW_SAFE_POSITION,
            targetPosition: ASSET_ROW_SAFE_POSITION,
          })
        })
      },
      /** Drag a row onto another row. */
      dragRow(from: number, to: Locator, force?: boolean) {
        return self.step(`Drag drive table row #${from} to custom locator`, (page) =>
          locateAssetRows(page)
            .nth(from)
            .dragTo(to, {
              sourcePosition: ASSET_ROW_SAFE_POSITION,
              ...(force == null ? {} : { force }),
            }),
        )
      },
      expandDirectory(index: number) {
        return self.step(`Expand drive table row #${index}`, async (page) => {
          const expandButton = locateAssetRows(page)
            .nth(index)
            .getByTestId('directory-row-expand-button')

          await expect(expandButton).toHaveAttribute('aria-label', TEXT.expand)

          await expandButton.click()
        })
      },
      collapseDirectory(index: number) {
        return self.step(`Collapse drive table row #${index}`, async (page) => {
          const collapseButton = locateAssetRows(page)
            .nth(index)
            .getByTestId('directory-row-expand-button')

          await expect(collapseButton).toHaveAttribute('aria-label', TEXT.collapse)

          return collapseButton.click()
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
    ).into(StartModalActions<Context>)
  }

  /** Expect the "start" modal to be visible. */
  expectStartModal() {
    return this.into(StartModalActions<Context>).withStartModal(async (startModal) => {
      await expect(startModal).toBeVisible()
    })
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
    return this.step(
      'Create empty project',
      (page) => page.getByText(TEXT.newEmptyProject, { exact: true }).click(),
      // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
      // Uncomment once cloud execution in the browser is re-enabled.
    ) /* .into(EditorPageActions<Context>) */
  }

  // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
  // Delete once cloud execution in the browser is re-enabled.
  /** Create a new empty project. */
  newEmptyProjectTest() {
    return this.step('Create empty project', (page) =>
      page.getByText(TEXT.newEmptyProject, { exact: true }).click(),
    ).into(EditorPageActions<Context>)
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

  /** Show the Asset Panel. */
  showAssetPanel() {
    return this.step('Show asset panel', async (page) => {
      const isShown = await this.isAssetPanelShown(page)

      if (!isShown) {
        await this.toggleAssetPanel()
      }
    })
  }

  /** Hide the Asset Panel. */
  hideAssetPanel() {
    return this.step('Hide asset panel', async (page) => {
      const isShown = await this.isAssetPanelShown(page)

      if (isShown) {
        await this.toggleAssetPanel()
      }
    })
  }

  /** Toggle the Asset Panel open or closed. */
  toggleAssetPanel() {
    return this.step('Toggle asset panel', async (page) => {
      page.getByLabel('Asset Panel').locator('visible=true').click()
      await this.waitForAssetPanelShown(page)
    })
  }

  /**
   * Check if the Asset Panel is shown.
   */
  async isAssetPanelShown(page: Page) {
    return await page
      .getByTestId('asset-panel')
      .isVisible({ timeout: 0 })
      .then(
        () => true,
        () => false,
      )
  }

  /**
   * Wait for the Asset Panel to be shown and visually stable
   */
  async waitForAssetPanelShown(page: Page) {
    await page.getByTestId('asset-panel').waitFor({ state: 'visible' })
  }

  /** Show the description tab of the Asset Panel. */
  toggleDescriptionAssetPanel() {
    return this.step('Toggle description asset panel', async (page) => {
      await this.showAssetPanel()
      await page.getByTestId('asset-panel-tab-settings').click()
    })
  }

  /** Show the Docs tab of the Asset Panel. */
  toggleDocsAssetPanel() {
    return this.step('Toggle docs asset panel', async (page) => {
      await this.showAssetPanel()
      await page.getByTestId('asset-panel-tab-docs').click()
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
  withAssetPanel(callback: LocatorCallback<Context>) {
    return this.step('Interact with asset panel', async (page, context) => {
      await callback(locateAssetPanel(page), context)
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
