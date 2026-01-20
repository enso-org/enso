/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'integration-test/base'

test.skip(
  'export from remote to local (local+remote)',
  {
    annotation: {
      type: 'details',
      description: 'Blocked on implementation of mocks for `?presigned=true`',
    },
  },
  async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()
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
      })
  },
)

test.skip(
  'export from remote to local (drag) (local+remote)',
  {
    annotation: {
      type: 'details',
      description: 'Blocked on implementation of mocks for `?presigned=true`',
    },
  },
  async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()
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
      })
  },
)

test('export from local to remote (local+remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
    })
    .driveTable.closeProject(0)
    .driveTable.rightClickRow(0)
    .contextMenu.exportToCloud()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('export from local to remote (drag) (local+remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .goToCategory.local()
    .newEmptyProject()
    .waitForEditorToLoad()
    .goToPage.drive()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
    })
    .driveTable.closeProject(0)
    .driveTable.clickRow(0)
    .driveTable.dragRowToCategory(0, 'Cloud')
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})
