/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('create project from template', async ({ page }) => {
  await actions.locateStartModalButton(page).click()
  // The second "sample" is the first template.
  await actions.locateSamples(page).nth(1).click()
  await test.expect(actions.locateEditor(page)).toBeVisible()
  await test.expect(actions.locateSamples(page).first()).not.toBeVisible()
})
