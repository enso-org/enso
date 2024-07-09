/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('drive view', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions }) =>
      await pageActions
        .withDriveView(async view => {
          await test.expect(view).toBeVisible()
        })
        .driveTable.expectPlaceholderRow()
        .newEmptyProject()
        .do(async () => {
          await test.expect(actions.locateEditor(page)).toBeAttached()
        })
        .goToPage.drive()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
        .do(async () => {
          await test.expect(actions.locateAssetsTable(page)).toBeVisible()
        })
        .newEmptyProject()
        .do(async () => {
          await test.expect(actions.locateEditor(page)).toBeAttached()
        })
        .goToPage.drive()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(2)
        })
        // The last opened project needs to be stopped, to remove the toast notification notifying the
        // user that project creation may take a while. Previously opened projects are stopped when the
        // new project is created.
        .driveTable.withRows(async rows => {
          await actions.locateStopProjectButton(rows.nth(0)).click()
        })
    // FIXME(#10488): This test fails because the mock endpoint returns the project is opened,
    //                but it must be stopped first to delete the project.
    // Project context menu
    // .driveTable.rightClickRow(0)
    // .withContextMenus(async menus => {
    //   // actions.locateContextMenus(page)
    //   await test.expect(menus).toBeVisible()
    // })
    // .contextMenu.moveToTrash()
    // .driveTable.withRows(async rows => {
    //   await test.expect(rows).toHaveCount(1)
    // })
  )
)
