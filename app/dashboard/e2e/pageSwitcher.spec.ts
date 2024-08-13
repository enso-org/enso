/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('page switcher', ({ page }) =>
  actions
    .mockAllAndLogin({ page })
    // Create a new project so that the editor page can be switched to.
    .newEmptyProject()
    .do(async (thePage) => {
      await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
      await test.expect(actions.locateEditor(thePage)).toBeVisible()
    })
    .goToPage.drive()
    .do(async (thePage) => {
      await test.expect(actions.locateDriveView(thePage)).toBeVisible()
      await test.expect(actions.locateEditor(thePage)).not.toBeVisible()
    })
    .goToPage.editor()
    .do(async (thePage) => {
      await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
      await test.expect(actions.locateEditor(thePage)).toBeVisible()
    }),
)
