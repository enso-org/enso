/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import { mockAllAndLogin, TEXT } from './actions'

test.test('delete and restore', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.moveFolderToTrash()
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.restoreFromTrash()
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    }),
)

test.test('delete and restore (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Delete')
    .do(async (thePage) => {
      await thePage.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
    })
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Mod+R')
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await test.expect(rows).toHaveCount(1)
    }),
)
