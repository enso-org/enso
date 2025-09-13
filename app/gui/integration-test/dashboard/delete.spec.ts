/** @file Test copying, moving, cutting and pasting. */
import { expect, test } from 'playwright/test'

import { modModifier } from 'integration-test/dashboard/actions/BaseActions'
import { mockAllAndLogin, TEXT } from './actions'

test('delete (local)', ({ page }) =>
  mockAllAndLogin({ page })
    .goToCategory.local()
    .createFolder()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(1)
    })
    .driveTable.rightClickRow(0)
    .contextMenu.delete()
    .driveTable.expectPlaceholderRow())

test('delete and restore (remote)', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('delete and restore project (remote)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject()
    },
  })
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
    }))

test('delete and restore (keyboard) (remote)', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('delete and restore project (keyboard) (remote)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject()
    },
  })
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
    }))

test('clear trash (remote)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory()
      api.addDirectory()
      api.addProject()
      api.addProject()
      api.addFile()
      api.addSecret()
      api.addDatalink()
    },
  })
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
    }))

test('clear trash (without directories) (remote)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject()
      api.addProject()
      api.addFile()
      api.addSecret()
      api.addDatalink()
    },
  })
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
    }))
