/** @file Test the labels sidebar panel. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('labels', async ({ page }) => {
    await api.mockApi(page)
    await actions.login(page)

    // Screenshot #1: Empty labels panel
    await test.expect(actions.locateLabelsPanel(page)).toHaveScreenshot()

    // Screenshot #2: "Create label" modal
    await actions.locateNewLabelButton(page).click()
    await test.expect(actions.locateNewLabelModal(page)).toHaveScreenshot()
})
