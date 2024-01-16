/** @file Test downloading the app from the user menu. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('download app', async ({ page }) => {
    await actions.locateUserMenuButton(page).click()
    const downloadPromise = page.waitForEvent('download')
    await actions.locateDownloadAppButton(page).click()
    const download = await downloadPromise
    await download.cancel()
    test.expect(download.url()).toMatch(/^https:[/][/]objects.githubusercontent.com/)
})
