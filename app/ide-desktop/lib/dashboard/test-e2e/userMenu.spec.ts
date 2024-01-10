/** @file Test the user menu. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('user menu', async ({ page }) => {
    // User menu
    await actions.locateUserMenuButton(page).click()
    await test.expect(actions.locateUserMenu(page)).toBeVisible()
})
