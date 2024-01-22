/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('create empty project', async ({ page }) => {
  await actions.locateHomePageIcon(page).click()
  // The first "sample" is a button to create a new empty project.
  await actions.locateSamples(page).nth(0).click()
  await test.expect(actions.locateEditor(page)).toBeVisible()
})

test.test('create project from template', async ({ page }) => {
  await actions.locateHomePageIcon(page).click()
  // The second "sample" is the first template.
  await actions.locateSamples(page).nth(1).click()
  await test.expect(actions.locateEditor(page)).toBeVisible()
})
