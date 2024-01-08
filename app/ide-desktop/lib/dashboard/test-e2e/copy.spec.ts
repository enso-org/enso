/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'
import * as keyboard from './keyboard'

test.test('cut', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)
})

test.test('copy', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)

    await actions.locateNewFolderIcon(page).click()
    await actions.locateNewFolderIcon(page).click()
    const firstAssetRow = actions.locateAssetsTableRows(page).nth(1)
    await test.expect(firstAssetRow).toBeVisible()
    const secondAssetRow = actions.locateAssetsTableRows(page).nth(2)
    await test.expect(secondAssetRow).toBeVisible()

    await firstAssetRow.click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locateCopyButton(page).click()

    await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
    await secondAssetRow.click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locatePasteButton(page).click()
    const duplicatedAssetRow = actions.locateAssetsTableRows(page).nth(3)
    await test.expect(duplicatedAssetRow).toBeVisible()
    await test.expect(duplicatedAssetRow).toHaveText(/^New_Folder_2 [(]copy[)]/)
    const parentLeft = await actions.getAssetRowLeftPx(secondAssetRow)
    const childLeft = await actions.getAssetRowLeftPx(duplicatedAssetRow)
    test.expect(childLeft, 'child is indented further than parent').toBeGreaterThan(parentLeft)
})

test.test('paste', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)
})

test.test('duplicate', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)

    await actions.locateNewFolderIcon(page).click()
    const firstAssetRow = actions.locateAssetsTableRows(page).nth(1)
    await test.expect(firstAssetRow).toBeVisible()

    await firstAssetRow.click({ button: 'right' })
    await test.expect(actions.locateContextMenus(page)).toBeVisible()
    await actions.locateDuplicateButton(page).click()

    await test.expect(actions.locateContextMenus(page)).not.toBeVisible()
    const duplicatedAssetRow = actions.locateAssetsTableRows(page).nth(1)
    await test.expect(duplicatedAssetRow).toBeVisible()
    await test.expect(duplicatedAssetRow).toHaveText(/^New_Folder_1 [(]copy[)]/)
})

test.test('duplicate (keyboard)', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)

    await actions.locateNewFolderIcon(page).click()
    const firstAssetRow = actions.locateAssetsTableRows(page).nth(1)
    await test.expect(firstAssetRow).toBeVisible()

    await firstAssetRow.click()
    await page.keyboard.press((await keyboard.modKey(page)) + '+D')

    const duplicatedAssetRow = actions.locateAssetsTableRows(page).nth(1)
    await test.expect(duplicatedAssetRow).toBeVisible()
    await test.expect(duplicatedAssetRow).toHaveText(/^New_Folder_1 [(]copy[)]/)
})
