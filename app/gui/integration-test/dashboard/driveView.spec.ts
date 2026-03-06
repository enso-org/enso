/** @file Test the drive view. */
import { expect, test, type Locator } from 'integration-test/base'

import { TEXT } from '../actions'

/** Find a button to close the project. */

function locateStopProjectButton(page: Locator) {
  return page.getByLabel(TEXT.stopExecution)
}

test('drive view', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
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
    .driveTable.withRows(async (rows) => {
      await locateStopProjectButton(rows.nth(0)).click()
      await locateStopProjectButton(rows.nth(1)).click()
    })
    .driveTable.rightClickRow(1)
    .contextMenu.moveToTrash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})
