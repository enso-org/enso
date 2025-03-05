/** @file Test copying, moving, cutting and pasting. */
import { expect, test, type Page } from '@playwright/test'

import { mockAllAndLogin } from './actions'

/** Find a button for the "Trash" category. */
function locateTrashCategory(page: Page) {
  return page.getByLabel('Trash').locator('visible=true')
}

test('copy', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('copy (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('move', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('move (drag)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({ title: 'New Folder 1' })
      api.addDirectory({ title: 'New Folder 2' })
    },
  })
    .driveTable.dragRowToRow('New Folder 2', 'New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/])
    })
    .driveTable.openDirectory('New Folder 1')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 2/])
    }))

test('move to trash', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory()
      api.addDirectory()
    },
  })
    // NOTE: For some reason, `react-aria-components` causes drag-n-drop to break if `Mod` is still
    // held.
    .withModPressed((modActions) =>
      modActions.driveTable.clickRow('New Folder 1').driveTable.clickRow('New Folder 2'),
    )
    .driveTable.dragRow('New Folder 1', locateTrashCategory(page))
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/, /^New Folder 2/])
    }))

test('move (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('cut (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
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
    }))

test('duplicate', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({ title: 'New Project 1' })
    },
  })
    .driveTable.rightClickRow(0)
    .contextMenu.duplicate()
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1, 1: New Project 1 (copy)]
      await expect(rows).toHaveText([/^New Project 1/, /^New Project 1 [(]copy[)]/])
    }))

test('duplicate (keyboard)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({ title: 'New Project 1' })
    },
  })
    .driveTable.clickRow('New Project 1')
    .press('Mod+D')
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1 (copy), 1: New Project 1]
      await expect(rows).toHaveText([/^New Project 1/, /^New Project 1 [(]copy[)]/])
    }))
