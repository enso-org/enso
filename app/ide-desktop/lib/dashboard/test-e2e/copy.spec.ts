/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('copy', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 2, 2: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await actions.locateCopyButton(page).click()
  // Assets: [1: Folder 2 <copied>, 2: Folder 1]

  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await assetRows.nth(2).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await actions.locatePasteButton(page).click()
  // Assets: [1: Folder 2, 2: Folder 1, 3: Folder 2 (copy) <child { depth=1 }>]
  await test.expect(assetRows.nth(3)).toBeVisible()
  await test.expect(assetRows.nth(3)).toHaveText(/^New_Folder_2 [(]copy[)]/)
  test.expect(await assetRows.count()).toBe(4)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(3))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('copy (keyboard)', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 2, 2: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click()
  await actions.press(page, 'Mod+C')
  // Assets: [1: Folder 2 <copied>, 2: Folder 1]

  await assetRows.nth(2).click()
  await actions.press(page, 'Mod+V')
  // Assets: [1: Folder 2, 2: Folder 1, 3: Folder 2 (copy) <child { depth=1 }>]
  await test.expect(assetRows.nth(3)).toBeVisible()
  await test.expect(assetRows.nth(3)).toHaveText(/^New_Folder_2 [(]copy[)]/)
  test.expect(await assetRows.count()).toBe(4)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(3))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 2, 2: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await actions.locateCutButton(page).click()
  // Assets: [1: Folder 2 <cut>, 2: Folder 1]

  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await assetRows.nth(2).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await actions.locatePasteButton(page).click()
  // Assets: [1: Folder 1, 2: Folder 2 <child { depth=1 }>]
  await test.expect(assetRows.nth(2)).toBeVisible()
  await test.expect(assetRows.nth(2)).toHaveText(/^New_Folder_2/)
  test.expect(await assetRows.count()).toBe(3)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move (drag)', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 2, 2: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).dragTo(assetRows.nth(2))
  // Assets: [1: Folder 1, 2: Folder 2 <child { depth=1 }>]

  await test.expect(assetRows.nth(2)).toBeVisible()
  await test.expect(assetRows.nth(2)).toHaveText(/^New_Folder_2/)
  test.expect(await assetRows.count()).toBe(3)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move (keyboard)', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 2, 2: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click()
  await actions.press(page, 'Mod+X')
  // Assets: [1: Folder 2 <cut>, 2: Folder 1]

  await assetRows.nth(2).click()
  await actions.press(page, 'Mod+V')
  // Assets: [1: Folder 1, 2: Folder 2 <child { depth=1 }>]
  await test.expect(assetRows.nth(2)).toBeVisible()
  await test.expect(assetRows.nth(2)).toHaveText(/^New_Folder_2/)
  test.expect(await assetRows.count()).toBe(3)
  const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
  const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(2))
  test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('duplicate', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click({ button: 'right' })
  await test.expect(actions.locateContextMenus(page)).toBeVisible()
  await actions.locateDuplicateButton(page).click()
  // Assets: [1: Folder 1 (copy), 2: Folder 1]
  test.expect(await assetRows.count()).toBe(3)

  await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New_Folder_1 [(]copy[)]/)
})

test.test('duplicate (keyboard)', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  await actions.locateNewFolderIcon(page).click()
  // Assets: [1: Folder 1]
  const assetRows = actions.locateAssetsTableRows(page)

  await assetRows.nth(1).click()
  await actions.press(page, 'Mod+D')
  // Assets: [1: Folder 1 (copy), 2: Folder 1]

  await test.expect(assetRows.nth(1)).toBeVisible()
  await test.expect(assetRows.nth(1)).toHaveText(/^New_Folder_1 [(]copy[)]/)
  test.expect(await assetRows.count()).toBe(3)
})
