/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'integration-test/base'

import { TEXT } from '../actions'

test('copy', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    // Assets: [0: New Folder 1]
    .createFolder()
    // Assets: [0: New Folder 2, 1: New Folder 1]
    .createFolder()
    .driveTable.rightClickRow('New Folder 2')
    // Assets: [0: New Folder 2 <copied>, 1: New Folder 1]
    .contextMenu.copy()
    .driveTable.rightClickRow('New Folder 1')
    // Assets: [0: New Folder 2, 1: New Folder 1, 2: New Folder 2 (copy) <child { depth=1 }>]
    .contextMenu.paste()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/, /^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2 \(copy\)/])
    })
})

test('copy (keyboard)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    // Assets: [0: New Folder 1]
    .createFolder()
    // Assets: [0: New Folder 2, 1: New Folder 1]
    .createFolder()
    .driveTable.clickRow('New Folder 2')
    // Assets: [0: New Folder 2 <copied>, 1: New Folder 1]
    .press('Mod+C')
    .driveTable.clickRow('New Folder 1')
    // Assets: [0: New Folder 2, 1: New Folder 1, 2: New Folder 2 (copy) <child { depth=1 }>]
    .press('Mod+V')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/, /^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2 \(copy\)/])
    })
})

test('move', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    // Assets: [0: New Folder 1]
    .createFolder()
    // Assets: [0: New Folder 2, 1: New Folder 1]
    .createFolder()
    .driveTable.rightClickRow('New Folder 2')
    // Assets: [0: New Folder 2 <cut>, 1: New Folder 1]
    .contextMenu.cut()
    .driveTable.rightClickRow('New Folder 1')
    // Assets: [0: New Folder 1, 1: New Folder 2 <child { depth=1 }>]
    .contextMenu.paste()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/])
    })
})

test('move (drag)', async ({ drivePage, cloudApi }) => {
  cloudApi.addDirectory({ title: 'New Folder 1' })
  cloudApi.addDirectory({ title: 'New Folder 2' })
  await drivePage.goToCategory
    .cloud()
    .driveTable.dragRowToRow('New Folder 2', 'New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/])
    })
})

test('move to trash', async ({ drivePage, cloudApi }) => {
  cloudApi.addDirectory()
  cloudApi.addDirectory()
  await drivePage.goToCategory
    .cloud()
    // NOTE: For some reason, `react-aria-components` causes drag-n-drop to break if `Mod` is still
    // held.
    .withModPressed((modActions) =>
      modActions.driveTable.clickRow('New Folder 1').driveTable.clickRow('New Folder 2'),
    )
    .driveTable.dragRowToCategory('New Folder 1', 'Trash')
    .do(async (page) => {
      // Confirm the deletion in the dialog
      await page.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
    })
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      // NOTE: the order of the rows is not guaranteed, because it depends on the modification time,
      // and requests may be executed in arbitrary order.
      const folder1 = rows.filter({ hasText: /New Folder 1/ })
      const folder2 = rows.filter({ hasText: /New Folder 2/ })

      await expect(folder1).toBeVisible()
      await expect(folder2).toBeVisible()
    })
})

test('move (keyboard)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    // Assets: [0: New Folder 1]
    .createFolder()
    // Assets: [0: New Folder 2, 1: New Folder 1]
    .createFolder()
    .driveTable.clickRow('New Folder 2')
    // Assets: [0: New Folder 2 <cut>, 1: New Folder 1]
    .press('Mod+X')
    .driveTable.clickRow('New Folder 1')
    // Assets: [0: New Folder 1, 1: New Folder 2 <child { depth=1 }>]
    .press('Mod+V')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/])
    })
})

test('cut (keyboard)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .createFolder()
    .driveTable.clickRow(0)
    .press('Mod+X')
    .driveTable.withRows(async (rows) => {
      // This action is not a builtin `expect` action, so it needs to be manually retried.
      await expect(async () => {
        expect(
          await rows.nth(0).evaluate((el) => Number(getComputedStyle(el).opacity)),
        ).toBeLessThan(1)
      }).toPass()
    })
})

test('duplicate', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject({ title: 'New Project 1' })
  await drivePage.goToCategory
    .cloud()
    .driveTable.rightClickRow(0)
    .contextMenu.duplicate()
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1, 1: New Project 1 (copy)]
      const project1 = rows.filter({ hasText: /New Project 1/, hasNotText: /\(copy\)/ })
      const project1Copy = rows.filter({ hasText: /New Project 1 \(copy\)/ })

      await expect(project1).toBeVisible()
      await expect(project1Copy).toBeVisible()
    })
})

test('duplicate (keyboard)', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject({ title: 'New Project 1' })
  await drivePage.goToCategory
    .cloud()
    .driveTable.clickRow('New Project 1')
    .press('Mod+D')
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1 (copy), 1: New Project 1]
      const project1 = rows.filter({ hasText: /New Project 1/, hasNotText: /\(copy\)/ })
      const project1Copy = rows.filter({ hasText: /New Project 1 \(copy\)/ })

      await expect(project1).toBeVisible()
      await expect(project1Copy).toBeVisible()
    })
})
