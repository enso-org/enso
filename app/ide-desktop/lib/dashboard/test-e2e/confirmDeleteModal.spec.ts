/** @file Test the confirm delete modal. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('confirm delete modal', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)

    // Screenshot #1: Delete modal
    // Initially, the table contains the header row and the placeholder row.
    await actions.locateNewProjectButton(page).click()
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const firstAssetRow = (await actions.locateAssetsTableRows(page).all())[1]!
    test.expect(firstAssetRow).not.toBeUndefined()
    // The project needs to be stopped, to remove the toast notification notifying the user that
    // project creation may take a while.
    await actions.locateStopProjectButton(firstAssetRow).click()
    await firstAssetRow.click({ button: 'right' })
    const contextMenu = actions.locateContextMenus(page)
    await actions.locateMoveToTrashButton(contextMenu).click()
    await test.expect(actions.locateConfirmDeleteModal(page)).toHaveScreenshot()
})
