/** @file Test the user menu. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('user menu', async ({ page }) => {
    await api.mockApi(page)
    await actions.login(page)

    // User menu
    await actions.locateUserMenuButton(page).click()
    await test.expect(actions.locateUserMenu(page)).toBeVisible()
})
