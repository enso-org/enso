/** @file Test the setup flow. */
import { expect, test } from '@playwright/test'

import { Plan } from 'enso-common/src/services/Backend'
import { mockAll } from './actions'

// Reset storage state for this file to avoid being authenticated
test.use({ storageState: { cookies: [], origins: [] } })

test('setup (free plan)', ({ page }) =>
  mockAll({
    page,
    setupAPI: (api) => {
      api.setCurrentUser(null)
    },
  })
    .loginAsNewUser()
    .setUsername('test user')
    .stayOnFreePlan()
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await expect(drive).toBeVisible()
    }))

test('setup (solo plan)', ({ page }) =>
  mockAll({
    page,
    setupAPI: (api) => {
      api.setCurrentUser(null)
    },
  })
    .loginAsNewUser()
    .setUsername('test user')
    .selectSoloPlan()
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await expect(drive).toBeVisible()
    }))

test('setup (team plan, skipping invites)', ({ page }) =>
  mockAll({
    page,
    setupAPI: (api) => {
      api.setCurrentUser(null)
    },
  })
    .loginAsNewUser()
    .setUsername('test user')
    .selectTeamPlan(Plan.team)
    .setOrganizationName('test organization')
    .skipInvitingUsers()
    .setTeamName('test team')
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await expect(drive).toBeVisible()
    }))

test('setup (team plan)', ({ page }) =>
  mockAll({
    page,
    setupAPI: (api) => {
      api.setCurrentUser(null)
    },
  })
    .loginAsNewUser()
    .setUsername('test user')
    .selectTeamPlan(Plan.team, 10)
    .setOrganizationName('test organization')
    .inviteUsers('foo@bar.baz bar@bar.baz, baz@example.com; other+email@org.co.uk')
    .setTeamName('test team')
    .goToPage.drive()
    .withDriveView(async (drive) => {
      await expect(drive).toBeVisible()
    }))

// No test for enterprise plan as the plan must be set to enterprise manually.
