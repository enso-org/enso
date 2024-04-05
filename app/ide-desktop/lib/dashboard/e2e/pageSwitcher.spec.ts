/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('page switcher', async ({ page }) => {
  // Create a new project so that the editor page can be switched to.
  await actions.locateNewProjectButton(page).click()
  // The current page is now the editor page.

  await actions.locateDrivePageIcon(page).click()
  await actions.expectOnScreen(actions.locateDriveView(page))
  await actions.expectNotOnScreen(actions.locateSamplesList(page))
  await actions.expectNotOnScreen(actions.locateEditor(page))

  // Clicking the current page switches to the home page.
  await actions.locateDrivePageIcon(page).click()
  await actions.expectNotOnScreen(actions.locateDriveView(page))
  await actions.expectOnScreen(actions.locateSamplesList(page))
  await actions.expectNotOnScreen(actions.locateEditor(page))

  await actions.locateEditorPageIcon(page).click()
  await actions.expectNotOnScreen(actions.locateDriveView(page))
  await actions.expectNotOnScreen(actions.locateSamplesList(page))
  await actions.expectOnScreen(actions.locateEditor(page))
})
