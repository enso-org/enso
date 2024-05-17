/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('delete and restore', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      .createFolder()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
      .driveTable.rightClickRow(0)
      .contextMenu.moveToTrash()
      .driveTable.expectPlaceholderRow()
      .goToTrashCategory()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
      .driveTable.rightClickRow(0)
      .contextMenu.restoreFromTrash()
      .driveTable.expectTrashPlaceholderRow()
      .goToHomeCategory()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
  )
)

test.test('delete and restore (keyboard)', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      .createFolder()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
      .driveTable.clickRow(0)
      .press('Delete')
      .driveTable.expectPlaceholderRow()
      .goToTrashCategory()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
      .driveTable.clickRow(0)
      .press('Mod+R')
      .driveTable.expectTrashPlaceholderRow()
      .goToHomeCategory()
      .driveTable.withRows(async rows => {
        await test.expect(rows).toHaveCount(1)
      })
  )
)
