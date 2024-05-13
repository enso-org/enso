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
  await test.test.step('initial state', () => {
    test.expect(api.currentOrganization?.name).toBe(api.defaultOrganizationName)
    test.expect(api.currentOrganization?.email).toBe(null)
    test.expect(api.currentOrganization?.picture).toBe(null)
    test.expect(api.currentOrganization?.website).toBe(null)
    test.expect(api.currentOrganization?.address).toBe(null)
  })

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another organization-name'
  await test.test.step('set name', async () => {
    await nameInput.fill(newName)
    await nameInput.press('Enter')
    test.expect(api.currentOrganization?.name).toBe(newName)
    test.expect(api.currentUser?.name).not.toBe(newName)
  })

  await test.test.step('unset name (should fail)', async () => {
    await nameInput.fill('')
    await nameInput.press('Enter')
    test.expect(api.currentOrganization?.name).toBe(newName)
    await test.expect(nameInput).toHaveValue(newName)
  })

  const invalidEmail = 'invalid@email'
  const emailInput = localActions.locateEmailInput(page)

  await test.test.step('set invalid email', async () => {
    await emailInput.fill(invalidEmail)
    await emailInput.press('Enter')
    test.expect(api.currentOrganization?.email).toBe(null)
  })

  const newEmail = 'organization@email.com'

  await test.test.step('set email', async () => {
    await emailInput.fill(newEmail)
    await emailInput.press('Enter')
    test.expect(api.currentOrganization?.email).toBe(newEmail)
    await test.expect(emailInput).toHaveValue(newEmail)
  })

  const websiteInput = localActions.locateWebsiteInput(page)
  const newWebsite = 'organization.org'

  // NOTE: It's not yet possible to unset the website or the location.
  await test.test.step('set website', async () => {
    await websiteInput.fill(newWebsite)
    await websiteInput.press('Enter')
    test.expect(api.currentOrganization?.website).toBe(newWebsite)
    await test.expect(websiteInput).toHaveValue(newWebsite)
  })

  const locationInput = localActions.locateLocationInput(page)
  const newLocation = 'Somewhere, CA'

  await test.test.step('set location', async () => {
    await locationInput.fill(newLocation)
    await locationInput.press('Enter')
    test.expect(api.currentOrganization?.address).toBe(newLocation)
    await test.expect(locationInput).toHaveValue(newLocation)
  })
})
