/** @file Test the organization settings tab. */
import * as test from '@playwright/test'

import { Plan } from 'enso-common/src/services/Backend'
import * as actions from './actions'

test.test('organization settings', async ({ page }) => {
  const api = await actions.mockAllAndLoginAndExposeAPI({
    page,
    setupAPI: (theApi) => {
      theApi.setPlan(Plan.team)
    },
  })
  const localActions = actions.settings.organization

  // Setup
  api.setCurrentOrganization(api.defaultOrganization)
  await test.test.step('Initial state', () => {
    test.expect(api.currentOrganization()?.name).toBe(api.defaultOrganizationName)
    test.expect(api.currentOrganization()?.email).toBe(null)
    test.expect(api.currentOrganization()?.picture).toBe(null)
    test.expect(api.currentOrganization()?.website).toBe(null)
    test.expect(api.currentOrganization()?.address).toBe(null)
  })
  await test.expect(page.getByText('Logging in to Enso...')).not.toBeVisible()

  await localActions.go(page)
  const nameInput = localActions.locateNameInput(page)
  const newName = 'another organization-name'
  await test.test.step('Set name', async () => {
    await nameInput.fill(newName)
    await nameInput.press('Enter')
    test.expect(api.currentOrganization()?.name).toBe(newName)
    test.expect(api.currentUser()?.name).not.toBe(newName)
  })

  await test.test.step('Unset name (should fail)', async () => {
    await nameInput.fill('')
    await nameInput.press('Enter')
    await test.expect(nameInput).toHaveValue('')
    test.expect(api.currentOrganization()?.name).toBe(newName)
  })

  const invalidEmail = 'invalid@email'
  const emailInput = localActions.locateEmailInput(page)

  await test.test.step('Set invalid email', async () => {
    await emailInput.fill(invalidEmail)
    await emailInput.press('Enter')
    test.expect(api.currentOrganization()?.email).toBe(null)
  })

  const newEmail = 'organization@email.com'

  await test.test.step('Set email', async () => {
    await emailInput.fill(newEmail)
    await emailInput.press('Enter')
    test.expect(api.currentOrganization()?.email).toBe(newEmail)
    await test.expect(emailInput).toHaveValue(newEmail)
  })

  const websiteInput = localActions.locateWebsiteInput(page)
  const newWebsite = 'organization.org'

  // NOTE: It's not yet possible to unset the website or the location.
  await test.test.step('Set website', async () => {
    await websiteInput.fill(newWebsite)
    await websiteInput.press('Enter')
    test.expect(api.currentOrganization()?.website).toBe(newWebsite)
    await test.expect(websiteInput).toHaveValue(newWebsite)
  })

  const locationInput = localActions.locateLocationInput(page)
  const newLocation = 'Somewhere, CA'

  await test.test.step('Set location', async () => {
    await locationInput.fill(newLocation)
    await locationInput.press('Enter')
    test.expect(api.currentOrganization()?.address).toBe(newLocation)
    await test.expect(locationInput).toHaveValue(newLocation)
  })
})

test.test('upload organization profile picture', async ({ page }) => {
  const api = await actions.mockAllAndLoginAndExposeAPI({
    page,
    setupAPI: (theApi) => {
      theApi.setPlan(Plan.team)
    },
  })
  const localActions = actions.settings.organizationProfilePicture

  await localActions.go(page)
  const fileChooserPromise = page.waitForEvent('filechooser')
  await localActions.locateInput(page).click()
  const fileChooser = await fileChooserPromise
  const name = 'bar.jpeg'
  const content = 'organization profile picture'
  await fileChooser.setFiles([{ name, buffer: Buffer.from(content), mimeType: 'image/jpeg' }])
  await test
    .expect(() => {
      test.expect(api.currentOrganizationProfilePicture()).toEqual(content)
    })
    .toPass()
})
