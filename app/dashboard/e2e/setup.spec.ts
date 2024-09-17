/** @file Test the setup flow. */
import * as test from '@playwright/test'

import { Plan } from 'enso-common/src/services/Backend'
import * as actions from './actions'

test.test('setup (free plan)', ({ page }) =>
  actions
    .mockAll({
      page,
      setupAPI: (api) => {
        api.setCurrentUser(null)
        api.setPlan(Plan.free)
      },
    })
    .loginAsNewUser()
    .setUsername('test user')
    .stayOnFreePlan()
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await test.expect(drive).toBeVisible()
    }),
)

test.test('setup (solo plan)', ({ page }) =>
  actions
    .mockAll({
      page,
      setupAPI: (api) => {
        api.setCurrentUser(null)
        api.setPlan(Plan.free)
      },
    })
    .loginAsNewUser()
    .setUsername('test user')
    .selectSoloPlan()
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await test.expect(drive).toBeVisible()
    }),
)

test.test('setup (team plan)', ({ page }) =>
  actions
    .mockAll({
      page,
      setupAPI: (api) => {
        api.setCurrentUser(null)
        api.setPlan(Plan.free)
      },
    })
    .loginAsNewUser()
    .setUsername('test user')
    .selectPlan(Plan.team)
    .setOrganizationName('test organization')
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await test.expect(drive).toBeVisible()
    }),
)
