/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('create empty project', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      .goToHomePage()
      .createEmptyProject()
      .do(async thePage => {
        await test.expect(actions.locateEditor(thePage)).toBeVisible()
      })
  )
)

test.test('create project from template', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions }) =>
    pageActions
      .goToHomePage()
      .createProjectFromTemplate(0)
      .do(async thePage => {
        await test.expect(actions.locateEditor(thePage)).toBeVisible()
      })
  )
)
