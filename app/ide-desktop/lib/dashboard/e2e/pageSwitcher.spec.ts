/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('page switcher', async ({ page }) => {
  const { pageActions } = await actions.mockAllAndLogin({ page })
  // Create a new project so that the editor page can be switched to.
  await actions.locateNewProjectButton(page).click()
  // The current page is now the editor page.

  await actions.locateDrivePageIcon(page).click()
  await test.expect(actions.locateDriveView(page)).toBeVisible()
  await test.expect(actions.locateSamplesList(page)).not.toBeVisible()
  await test.expect(actions.locateEditor(page)).not.toBeVisible()

  await actions.locateHomePageIcon(page).click()
  await test.expect(actions.locateDriveView(page)).not.toBeVisible()
  await test.expect(actions.locateSamplesList(page)).toBeVisible()
  await test.expect(actions.locateEditor(page)).not.toBeVisible()

  await pageActions.goToEditorPage().done()
  await test.expect(actions.locateDriveView(page)).not.toBeVisible()
  await test.expect(actions.locateSamplesList(page)).not.toBeVisible()
  await test.expect(actions.locateEditor(page)).toBeVisible()
})
