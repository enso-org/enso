/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'integration-test/base'

import { modModifier } from 'integration-test/actions/BaseActions'
import { TEXT } from '../actions'

test('delete (local)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .goToCategory.local()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.delete()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('delete and restore (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.moveToTrash()
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.restoreFromTrash()
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('delete and restore project (remote)', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject()
  await drivePage.goToCategory
    .cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.moveToTrash()
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.restoreFromTrash()
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('delete and restore (keyboard) (remote)', async ({ drivePage }) => {
  await drivePage.goToCategory
    .cloud()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Delete')
    .do(async (thePage) => {
      await thePage.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
    })
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Mod+R')
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('delete and restore project (keyboard) (remote)', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject()
  await drivePage.goToCategory
    .cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Delete')
    .do(async (thePage) => {
      await thePage.getByRole('button', { name: TEXT.delete }).getByText(TEXT.delete).click()
    })
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.clickRow(0)
    .press('Mod+R')
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
})

test('clear trash (remote)', async ({ drivePage, cloudApi }) => {
  cloudApi.addDirectory()
  cloudApi.addDirectory()
  cloudApi.addProject()
  cloudApi.addProject()
  cloudApi.addFile()
  cloudApi.addSecret()
  cloudApi.addDatalink()
  await drivePage.goToCategory
    .cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(7)
    })
    .driveTable.withRows(async (rows, _nonRows, _context, page) => {
      const mod = await modModifier(page)
      // Parallelizing this using `Promise.all` makes it inconsistent.
      const rowEls = await rows.all()
      for (const row of rowEls) {
        await row.click({ modifiers: [mod] })
      }
    })
    .driveTable.rightClickRow(0)
    .contextMenu.moveToTrash()
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(7)
    })
    .clearTrash()
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(0)
    })
})

test('clear trash (without directories) (remote)', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject()
  cloudApi.addProject()
  cloudApi.addFile()
  cloudApi.addSecret()
  cloudApi.addDatalink()
  await drivePage.goToCategory
    .cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(5)
    })
    .driveTable.withRows(async (rows, _nonRows, _context, page) => {
      const mod = await modModifier(page)
      // Parallelizing this using `Promise.all` makes it inconsistent.
      const rowEls = await rows.all()
      for (const row of rowEls) {
        await row.click({ modifiers: [mod] })
      }
    })
    .driveTable.rightClickRow(0)
    .contextMenu.moveToTrash()
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(5)
    })
    .clearTrash()
    .driveTable.expectTrashPlaceholderRow()
    .goToCategory.cloud()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(0)
    })
})
