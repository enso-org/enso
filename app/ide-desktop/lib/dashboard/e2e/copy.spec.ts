/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('copy', async ({ page }) => {
    const assetRows = actions.locateAssetRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 1]
    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 2, 1: Folder 1]
    await assetRows.nth(0).click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locateCopyButton(page).click()
    // Assets: [0: Folder 2 <copied>, 1: Folder 1]
    await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
    await assetRows.nth(1).click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locatePasteButton(page).click()
    // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
    await test.expect(assetRows).toHaveCount(3)
    await test.expect(assetRows.nth(2)).toBeVisible()
    await test.expect(assetRows.nth(2)).toHaveText(/^New_Folder_2 [(]copy[)]/)
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
    await assetRows.nth(0).click()
    await actions.press(page, 'Mod+C')
    // Assets: [0: Folder 2 <copied>, 1: Folder 1]
    await assetRows.nth(1).click()
    await actions.press(page, 'Mod+V')
    // Assets: [0: Folder 2, 1: Folder 1, 2: Folder 2 (copy) <child { depth=1 }>]
    await test.expect(assetRows).toHaveCount(3)
    await test.expect(assetRows.nth(2)).toBeVisible()
    await test.expect(assetRows.nth(2)).toHaveText(/^New_Folder_2 [(]copy[)]/)
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
    await actions.locateCutButton(page).click()
    // Assets: [0: Folder 2 <cut>, 1: Folder 1]
    await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
    await assetRows.nth(1).click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locatePasteButton(page).click()
    // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
    await test.expect(assetRows).toHaveCount(2)
    await test.expect(assetRows.nth(1)).toBeVisible()
    await test.expect(assetRows.nth(1)).toHaveText(/^New_Folder_2/)
    const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
    const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
    test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move (drag)', async ({ page }) => {
    const assetRows = actions.locateAssetRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [1: Folder 1]
    await actions.locateNewFolderIcon(page).click()
    // Assets: [1: Folder 2, 2: Folder 1]
    await assetRows.nth(0).dragTo(assetRows.nth(1))
    // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
    await test.expect(assetRows).toHaveCount(2)
    await test.expect(assetRows.nth(1)).toBeVisible()
    await test.expect(assetRows.nth(1)).toHaveText(/^New_Folder_2/)
    const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
    const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
    test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('move (keyboard)', async ({ page }) => {
    const assetRows = actions.locateAssetRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 1]
    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 2, 1: Folder 1]
    await assetRows.nth(0).click()
    await actions.press(page, 'Mod+X')
    // Assets: [0: Folder 2 <cut>, 1: Folder 1]
    await assetRows.nth(1).click()
    await actions.press(page, 'Mod+V')
    // Assets: [0: Folder 1, 1: Folder 2 <child { depth=1 }>]
    await test.expect(assetRows).toHaveCount(2)
    await test.expect(assetRows.nth(1)).toBeVisible()
    await test.expect(assetRows.nth(1)).toHaveText(/^New_Folder_2/)
    const parentLeft = await actions.getAssetRowLeftPx(assetRows.nth(0))
    const childLeft = await actions.getAssetRowLeftPx(assetRows.nth(1))
    test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('duplicate', async ({ page }) => {
    const assetRows = actions.locateAssetRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 1]
    await assetRows.nth(0).click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locateDuplicateButton(page).click()
    // Assets: [0: Folder 1 (copy), 1: Folder 1]
    await test.expect(assetRows).toHaveCount(2)
    await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
    await test.expect(assetRows.nth(0)).toBeVisible()
    await test.expect(assetRows.nth(0)).toHaveText(/^New_Folder_1 [(]copy[)]/)
})

test.test('duplicate (keyboard)', async ({ page }) => {
    const assetRows = actions.locateAssetRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 1]
    await assetRows.nth(0).click()
    await actions.press(page, 'Mod+D')
    // Assets: [0: Folder 1 (copy), 1: Folder 1]
    await test.expect(assetRows).toHaveCount(2)
    await test.expect(assetRows.nth(0)).toBeVisible()
    await test.expect(assetRows.nth(0)).toHaveText(/^New_Folder_1 [(]copy[)]/)
})
