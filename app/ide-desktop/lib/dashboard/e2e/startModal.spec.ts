/** @file Test the "change password" modal. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test('create project from template', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions }) =>
      await pageActions
        .openStartModal()
        .createProjectFromTemplate(0)
        .do(async thePage => {
          await test.expect(actions.locateEditor(thePage)).toBeVisible()
          await test.expect(actions.locateSamples(page).first()).not.toBeVisible()
        })
  )
)
