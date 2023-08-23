/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

/* eslint-disable @typescript-eslint/no-magic-numbers */

test.test('drive view', async ({ page }) => {
    await api.mockApi(page)
    await actions.mockDate(page)
    await actions.login(page)

    // Screenshot #1: Drive view
    // Initially, the table contains the header row and the placeholder row.
    await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(2)
    await test.expect(actions.locateDriveView(page)).toHaveScreenshot()

    // Screenshot #1: Assets table with one asset
    await actions.locateNewProjectButton(page).click()
    // The placeholder row becomes hidden.
    await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(2)
    await test.expect(actions.locateAssetsTable(page)).toHaveScreenshot()

    await actions.locateNewProjectButton(page).click()
    await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(3)
})
