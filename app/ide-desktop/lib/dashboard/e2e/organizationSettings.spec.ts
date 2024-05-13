/** @file Test the organization settings tab. */
import * as test from '@playwright/test'

import type * as backend from '#/services/Backend'

import * as actions from './actions'

test.test('organization settings', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const localActions = actions.settings.organization

  // Setup
  const initialOrganization: backend.OrganizationInfo = {
    id: api.defaultOrganizationId,
    name: api.defaultOrganizationName,
    address: null,
    email: null,
    picture: null,
    website: null,
  }
  api.setCurrentOrganization(initialOrganization)
  test.expect(api.currentOrganization?.name).toBe(api.defaultOrganizationName)

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another organization-name'
  await nameInput.fill(newName)
  await nameInput.press('Enter')
  test.expect(api.currentOrganization?.name).toBe(newName)
  test.expect(api.currentUser?.name).not.toBe(newName)
})
