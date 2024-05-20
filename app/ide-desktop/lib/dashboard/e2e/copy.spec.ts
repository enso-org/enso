/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

// =====================
// === Local actions ===
// =====================

// These actions have been migrated to the new API, and are included here as a temporary measure
// until this file is also migrated to the new API.

/** Find a "duplicate" button (if any) on the current page. */
export function locateDuplicateButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Duplicate' }).getByText('Duplicate')
}

/** Find a "copy" button (if any) on the current page. */
function locateCopyButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Copy' }).getByText('Copy')
}

/** Find a "cut" button (if any) on the current page. */
function locateCutButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Cut' }).getByText('Cut')
}

/** Find a "paste" button (if any) on the current page. */
function locatePasteButton(page: test.Locator | test.Page) {
  return page.getByRole('button', { name: 'Paste' }).getByText('Paste')
}

/** A test assertion to confirm that there is only one row visible, and that row is the
 * placeholder row displayed when there are no assets to show. */
export async function expectPlaceholderRow(page: test.Page) {
  const assetRows = actions.locateAssetRows(page)
  await test.test.step('Expect placeholder row', async () => {
    await test.expect(assetRows).toHaveCount(1)
    await test.expect(assetRows).toHaveText(/You have no files/)
  })
}

// =============
// === Tests ===
// =============

test.test.beforeEach(actions.mockAllAndLogin)

test.test('copy', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 2, 1: Folder 1]
  await assetRows.nth(0).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await locateCopyButton(page).click()
  // Assets: [0: Folder 2 <copied>, 1: Folder 1]
  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await assetRows.nth(1).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await locatePasteButton(page).click()
  // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
  await test.expect(assetRows).toHaveCount(3)
  await test.expect(assetRows.nth(2)).toBeVisible()
  await test.expect(assetRows.nth(2)).toHaveText(/^New Folder 2 [(]copy[)]/)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('copy (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 2, 1: Folder 1]
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.press(page, 'Mod+C')
  // Assets: [0: Folder 2 <copied>, 1: Folder 1]
  await actions.clickAssetRow(assetRows.nth(1))
  await actions.press(page, 'Mod+V')
  // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
  await test.expect(assetRows).toHaveCount(3)
  await test.expect(assetRows.nth(2)).toBeVisible()
  await test.expect(assetRows.nth(2)).toHaveText(/^New Folder 2 [(]copy[)]/)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 2, 1: Folder 1]
  await assetRows.nth(0).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await locateCutButton(page).click()
  // Assets: [0: Folder 2 <cut>, 1: Folder 1]
  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await assetRows.nth(1).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await locatePasteButton(page).click()
  // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New Folder 2/)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move (drag)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 2, 1: Folder 1]
  await actions.dragAssetRowToAssetRow(assetRows.nth(0), assetRows.nth(1))
  // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New Folder 2/)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move to trash', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  await actions.locateNewFolderIcon(page).click()
  await page.keyboard.down(await actions.modModifier(page))
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.clickAssetRow(assetRows.nth(1))
  // NOTE: For some reason, `react-aria-components` causes drag-n-drop to break if `Mod` is still
  // held.
  await page.keyboard.up(await actions.modModifier(page))
  await actions.dragAssetRow(assetRows.nth(0), actions.locateTrashCategory(page))
  await expectPlaceholderRow(page)
  await actions.locateTrashCategory(page).click()
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^New Folder 1/)
  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New Folder 2/)
})

test.test('move (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 2, 1: Folder 1]
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.press(page, 'Mod+X')
  // Assets: [0: Folder 2 <cut>, 1: Folder 1]
  await actions.clickAssetRow(assetRows.nth(1))
  await actions.press(page, 'Mod+V')
  // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New Folder 2/)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('cut (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.press(page, 'Mod+X')
  // This action is not a builtin `expect` action, so it needs to be manually retried.
  await test
    .expect(async () => {
      test
        .expect(await assetRows.nth(0).evaluate(el => Number(getComputedStyle(el).opacity)))
        .toBeLessThan(1)
    })
    .toPass()
})

test.test('duplicate', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await assetRows.nth(0).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await locateDuplicateButton(page).click()
  // Assets: [0: Folder 1 (copy), 1: Folder 1]
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^New Folder 1 [(]copy[)]/)
})

test.test('duplicate (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [0: Folder 1]
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.press(page, 'Mod+D')
  // Assets: [0: Folder 1 (copy), 1: Folder 1]
  await test.expect(assetRows).toHaveCount(2)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^New Folder 1 [(]copy[)]/)
})
