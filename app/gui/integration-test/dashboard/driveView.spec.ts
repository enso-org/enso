/** @file Test the drive view. */
import { expect, test, type Locator } from 'playwright/test'

import { TEXT, mockAllAndLogin } from './actions'

/** Find a button to close the project. */

function locateStopProjectButton(page: Locator) {
  return page.getByLabel(TEXT.stopExecution)
}

test('drive view', ({ page }) =>
  mockAllAndLogin({ page })
    .withDriveView(async (view) => {
      await expect(view).toBeVisible()
    })
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .withAssetsTable(async (assetsTable) => {
      await expect(assetsTable).toBeVisible()
    })
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
    })
    // The last opened project needs to be stopped, to remove the toast notification notifying the
    // user that project creation may take a while. Previously opened projects are stopped when the
    // new project is created.
    .driveTable.withRows(async (rows) => {
      await locateStopProjectButton(rows.nth(0)).click()
    })
    .driveTable.rightClickRow(1)
    .contextMenu.moveToTrash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))
