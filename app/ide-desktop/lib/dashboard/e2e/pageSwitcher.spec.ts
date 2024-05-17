/** @file Test the login flow. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('page switcher', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      // Create a new project so that the editor page can be switched to.
      .createProject()
      .goToDrivePage()
      .do(async thePage => {
        // TODO: completely remove global locators
        await test.expect(actions.locateDriveView(thePage)).toBeVisible()
        await test.expect(actions.locateSamplesList(thePage)).not.toBeVisible()
        await test.expect(actions.locateEditor(thePage)).not.toBeVisible()
      })
      .goToHomePage()
      .do(async thePage => {
        await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
        await test.expect(actions.locateSamplesList(thePage)).toBeVisible()
        await test.expect(actions.locateEditor(thePage)).not.toBeVisible()
      })
      .goToEditorPage()
      .do(async thePage => {
        await test.expect(actions.locateDriveView(thePage)).not.toBeVisible()
        await test.expect(actions.locateSamplesList(thePage)).not.toBeVisible()
        await test.expect(actions.locateEditor(thePage)).toBeVisible()
      })
  )
)
