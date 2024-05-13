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
  test.expect(api.currentOrganization?.email).toBe(null)
  test.expect(api.currentOrganization?.picture).toBe(null)
  test.expect(api.currentOrganization?.website).toBe(null)
  test.expect(api.currentOrganization?.address).toBe(null)

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another organization-name'
  await nameInput.fill(newName)
  await nameInput.press('Enter')
  test.expect(api.currentOrganization?.name).toBe(newName)
  test.expect(api.currentUser?.name).not.toBe(newName)

  // Setting to an empty name should fail.
  // FIXME: Add validation on the frontend side.
  await nameInput.fill('')
  await nameInput.press('Enter')
  test.expect(api.currentOrganization?.name).toBe(newName)
  await test.expect(nameInput).toHaveValue(newName)

  const invalidEmail = 'invalid@email'
  const emailInput = localActions.locateEmailInput(page)
  await emailInput.fill(invalidEmail)
  await emailInput.press('Enter')
  test.expect(api.currentOrganization?.name).toBe(null)

  const newEmail = 'organization@email.com'
  test.expect(api.currentOrganization?.name).toBe(newName)

  const websiteInput = localActions.locateWebsiteInput(page)
  const locationInput = localActions.locateLocationInput(page)
})
