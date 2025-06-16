/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from '@playwright/test'

import { mockAllAndLogin } from './actions'

test.skip(
  'export from remote to local (local+remote)',
  {
    annotation: {
      type: 'details',
      description: 'Blocked on implementation of mocks for `?presigned=true`',
    },
  },
  ({ page }) =>
    mockAllAndLogin({ page })
      .newEmptyProject()
      .waitForEditorToLoad()
      .goToPage.drive()
      .driveTable.withRows(async (rows) => {
        await expect(rows).toHaveCount(1)
      })
      .driveTable.closeProject(0)
      .driveTable.rightClickRow(0)
      .contextMenu.exportToLocal()
      .goToCategory.local()
      .driveTable.withRows(async (rows) => {
        await expect(rows).toHaveCount(1)
      }),
)

test.skip(
  'export from remote to local (drag) (local+remote)',
  {
    annotation: {
      type: 'details',
      description: 'Blocked on implementation of mocks for `?presigned=true`',
    },
  },
  ({ page }) =>
    mockAllAndLogin({ page })
      .newEmptyProject()
      .waitForEditorToLoad()
      .goToPage.drive()
      .driveTable.withRows(async (rows) => {
        await expect(rows).toHaveCount(1)
      })
      .driveTable.closeProject(0)
      .driveTable.clickRow(0)
      .driveTable.dragRowToCategory(0, 'Local')
      .goToCategory.local()
      .driveTable.withRows(async (rows) => {
        await expect(rows).toHaveCount(1)
      }),
)

test('export from local to remote (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.closeProject(0)
    .driveTable.rightClickRow(0)
    .contextMenu.exportToCloud()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))

test('export from local to remote (drag) (local+remote)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.closeProject(0)
    .driveTable.clickRow(0)
    .driveTable.dragRowToCategory(0, 'Cloud')
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    }))
