/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('cut', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)
})

test.test('copy', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)
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
