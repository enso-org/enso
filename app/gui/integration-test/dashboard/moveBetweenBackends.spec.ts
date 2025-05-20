/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from '@playwright/test'

import { mockAllAndLogin } from './actions'

test('move from remote to local (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.exportToLocal()
    .goToCategory.local()
    .driveTable.expectPlaceholderRow()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))

test('move from remote to local (drag) (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.dragRowToCategory.local(0)
    .goToCategory.local()
    .driveTable.expectPlaceholderRow()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))

test('move from local to remote (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.exportToCloud()
    .goToCategory.cloud()
    .driveTable.expectPlaceholderRow()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))

test('move from local to remote (drag) (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.dragRowToCategory.cloud(0)
    .goToCategory.cloud()
    .driveTable.expectPlaceholderRow()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))
