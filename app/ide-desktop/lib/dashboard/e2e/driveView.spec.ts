/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('drive view', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      .withDriveView(async view => {
        await test.expect(view).toBeVisible()
      })
      .driveTable.expectPlaceholderRow()
      .createProject()
      .do(async () => {
        await test.expect(actions.locateEditor(page)).toBeVisible()
      })
      .goToDrivePage()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
      .do(async () => {
        await test.expect(actions.locateAssetsTable(page)).toBeVisible()
      })
      .createProject()
      .do(async () => {
        await test.expect(actions.locateEditor(page)).toBeVisible()
      })
      .goToDrivePage()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(2)
      })
      // The last opened project needs to be stopped, to remove the toast notification notifying the
      // user that project creation may take a while. Previously opened projects are stopped when the
      // new project is created.
      .driveTable.withRows(async rows => {
        await actions.locateStopProjectButton(rows.nth(0)).click()
      })
      // Project context menu
      .driveTable.rightClickRow(0)
      .withContextMenus(async menus => {
        // actions.locateContextMenus(page)
        await test.expect(menus).toBeVisible()
      })
      .contextMenu.moveToTrash()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
  )
)
