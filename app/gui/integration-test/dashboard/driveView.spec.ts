/** @file Test the drive view. */
import { expect, test, type Locator } from '@playwright/test'

import { TEXT, mockAllAndLogin } from './actions'

/** Find a button to close the project. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function locateStopProjectButton(page: Locator) {
  return page.getByLabel(TEXT.stopExecution)
}

test('drive view', ({ page }) =>
  mockAllAndLogin({ page })
    .withDriveView(async (view) => {
      await expect(view).toBeVisible()
    })
    .driveTable.expectPlaceholderRow()
    .newEmptyProject()
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // .do(async () => {
    //   await expect(locateEditor(page)).toBeAttached()
    // })
    // .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .withAssetsTable(async (assetsTable) => {
      await expect(assetsTable).toBeVisible()
    })
    .newEmptyProject()
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // .do(async () => {
    //   await expect(locateEditor(page)).toBeAttached()
    // })
    // .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
    })
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // // The last opened project needs to be stopped, to remove the toast notification notifying the
    // // user that project creation may take a while. Previously opened projects are stopped when the
    // // new project is created.
    // .driveTable.withRows(async (rows) => {
    //   await locateStopProjectButton(rows.nth(1)).click()
    // })
    // Project context menu
    .driveTable.rightClickRow(0)
    .contextMenu.moveNonFolderToTrash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))
