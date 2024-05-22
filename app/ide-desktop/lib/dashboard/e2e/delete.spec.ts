/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('delete and restore', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions }) =>
      await pageActions
        .createFolder()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
        .driveTable.rightClickRow(0)
        .contextMenu.moveToTrash()
        .driveTable.expectPlaceholderRow()
        .goToCategory.trash()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
        .driveTable.rightClickRow(0)
        .contextMenu.restoreFromTrash()
        .driveTable.expectTrashPlaceholderRow()
        .goToCategory.home()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
  )
)

test.test('delete and restore (keyboard)', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions }) =>
      await pageActions
        .createFolder()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
        .driveTable.clickRow(0)
        .press('Delete')
        .driveTable.expectPlaceholderRow()
        .goToCategory.trash()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
        .driveTable.clickRow(0)
        .press('Mod+R')
        .driveTable.expectTrashPlaceholderRow()
        .goToCategory.home()
        .driveTable.withRows(async rows => {
          await test.expect(rows).toHaveCount(1)
        })
  )
)
