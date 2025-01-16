/** @file Test copying, moving, cutting and pasting. */
import { expect, test, type Locator, type Page } from '@playwright/test'

import { mockAllAndLogin } from './actions'

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

/** Find a button for the "Trash" category. */
function locateTrashCategory(page: Page) {
  return page.getByLabel('Trash').locator('visible=true')
}

/**
 * Get the left side of the bounding box of an asset row. The locator MUST be for an asset row.
 * DO NOT assume the left side of the outer container will change. This means that it is NOT SAFE
 * to do anything with the returned values other than comparing them.
 */
function getAssetRowLeftPx(locator: Locator) {
  return locator.evaluate((el) => el.children[0]?.children[0]?.getBoundingClientRect().left ?? 0)
}

test('copy', ({ page }) =>
  mockAllAndLogin({ page })
    // Assets: [0: Folder 1]
    .createFolder()
    // Assets: [0: Folder 2, 1: Folder 1]
    .createFolder()
    .driveTable.rightClickRow(1)
    // Assets: [0: Folder 2 <copied>, 1: Folder 1]
    .contextMenu.copy()
    .driveTable.rightClickRow(0)
    // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
    .contextMenu.paste()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(3)

      const child = rows.nth(1)
      const parent = rows.nth(0)

      await expect(child).toBeVisible()
      await expect(child).toHaveText(/^New Folder 1 [(]copy[)]*/)

      const parentLeft = await getAssetRowLeftPx(parent)
      const childLeft = await getAssetRowLeftPx(child)

      expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
    }))

test('copy (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    // Assets: [0: Folder 1]
    .createFolder()
    // Assets: [0: Folder 2, 1: Folder 1]
    .createFolder()
    .driveTable.clickRow(1)
    // Assets: [0: Folder 2 <copied>, 1: Folder 1]
    .press('Mod+C')
    .driveTable.clickRow(0)
    // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
    .press('Mod+V')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(3)

      const child = rows.nth(1)
      const parent = rows.nth(0)

      await expect(child).toBeVisible()
      await expect(child).toHaveText(/^New Folder 1 [(]copy[)]*/)

      const parentLeft = await getAssetRowLeftPx(parent)
      const childLeft = await getAssetRowLeftPx(child)

      expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
    }))

test('move', ({ page }) =>
  mockAllAndLogin({ page })
    // Assets: [0: Folder 1]
    .createFolder()
    // Assets: [0: Folder 2, 1: Folder 1]
    .createFolder()
    .driveTable.rightClickRow(0)
    // Assets: [0: Folder 2 <cut>, 1: Folder 1]
    .contextMenu.cut()
    .driveTable.rightClickRow(1)
    // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
    .contextMenu.paste()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)

      const child = rows.nth(1)
      const parent = rows.nth(0)

      await expect(child).toBeVisible()
      await expect(child).toHaveText(/^New Folder 2/)

      const parentLeft = await getAssetRowLeftPx(parent)
      const childLeft = await getAssetRowLeftPx(child)

      expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
    }))

test('move (drag)', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addDirectory({
        title: 'New Folder 1',
      })
      api.addDirectory({
        title: 'New Folder 2',
      })
    },
  })
    .driveTable.dragRowToRow(0, 1)
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)

      const child = rows.nth(1)
      const parent = rows.nth(0)

      await expect(child).toBeVisible()
      await expect(child).toHaveText(/^New Folder 1/)

      const parentLeft = await getAssetRowLeftPx(parent)
      const childLeft = await getAssetRowLeftPx(child)

      expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
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
    .withModPressed((modActions) => modActions.driveTable.clickRow(1).driveTable.clickRow(0))
    .driveTable.dragRow(0, locateTrashCategory(page))
    .driveTable.expectPlaceholderRow()
    .goToCategory.trash()
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([/^New Folder 1/, /^New Folder 2/])
    }))

test('move (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    // Assets: [0: Folder 1]
    .createFolder()
    // Assets: [0: Folder 2, 1: Folder 1]
    .createFolder()
    .driveTable.clickRow(0)
    // Assets: [0: Folder 2 <cut>, 1: Folder 1]
    .press('Mod+X')
    .driveTable.clickRow(1)
    // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
    .press('Mod+V')
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveCount(2)
      await expect(rows.nth(1)).toBeVisible()
      await expect(rows.nth(1)).toHaveText(/^New Folder 2/)
      const parentLeft = await getAssetRowLeftPx(rows.nth(0))
      const childLeft = await getAssetRowLeftPx(rows.nth(1))
      expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
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
  mockAllAndLogin({ page })
    // Assets: [0: New Project 1]
    .newEmptyProject()
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // .waitForEditorToLoad()
    // .goToPage.drive()
    .driveTable.rightClickRow(0)
    .contextMenu.duplicate()
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1, 1: New Project 1 (copy)]
      await expect(rows).toHaveCount(2)
      await expect(locateContextMenu(page)).not.toBeVisible()
      await expect(rows.nth(1)).toBeVisible()
      await expect(rows.nth(1)).toHaveText(/^New Project 1 [(]copy[)]/)
    }))

test('duplicate (keyboard)', ({ page }) =>
  mockAllAndLogin({ page })
    // Assets: [0: New Project 1]
    .newEmptyProject()
    // FIXME[sb]: https://github.com/enso-org/cloud-v2/issues/1615
    // Uncomment once cloud execution in the browser is re-enabled.
    // .waitForEditorToLoad()
    // .goToPage.drive()
    .driveTable.clickRow(0)
    .press('Mod+D')
    .driveTable.withRows(async (rows) => {
      // Assets: [0: New Project 1 (copy), 1: New Project 1]
      await expect(rows).toHaveCount(2)
      await expect(rows.nth(1)).toBeVisible()
      await expect(rows.nth(1)).toHaveText(/^New Project 1 [(]copy[)]/)
    }))
