/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('create folder', async ({ page }) => {
    const assetRows = actions.locateAssetsTableRows(page)

    await actions.locateNewFolderIcon(page).click()
    // Assets: [0: Folder 1]
    await test.expect(assetRows).toHaveCount(1)
    await test.expect(assetRows.nth(0)).toBeVisible()
    await test.expect(assetRows.nth(0)).toHaveText(/^New_Folder_1/)
})

test.test('create project', async ({ page }) => {
    const assetRows = actions.locateAssetsTableRows(page)

    await actions.locateNewProjectButton(page).click()
    // Assets: [0: Project 1]
    await test.expect(assetRows).toHaveCount(1)
    await page.evaluate(() => {
        const ideContainer = document.getElementById('root')
        if (ideContainer) {
            ideContainer.style.height = '100vh'
        }
    })
    await test.expect(actions.locateEditor(page)).toBeVisible()
})

test.test('create secret', async ({ page }) => {
    const assetRows = actions.locateAssetsTableRows(page)

    await actions.locateNewSecretIcon(page).click()
    const name = 'a secret name'
    const value = 'a secret value'
    await actions.locateSecretNameInput(page).fill(name)
    await actions.locateSecretValueInput(page).fill(value)
    await actions.locateCreateButton(page).click()
    await test.expect(assetRows).toHaveCount(1)
    await test.expect(assetRows.nth(0)).toBeVisible()
    await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + name))
})
