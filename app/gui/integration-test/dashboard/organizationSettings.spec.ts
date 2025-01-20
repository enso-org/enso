/** @file Test the organization settings tab. */
import { expect, test } from '@playwright/test'

import { Plan } from 'enso-common/src/services/Backend'
import { mockAllAndLogin } from './actions'

const NEW_NAME = 'another organization-name'
const INVALID_EMAIL = 'invalid@email'
const NEW_EMAIL = 'organization@email.com'
const NEW_WEBSITE = 'organization.org'
const NEW_LOCATION = 'Somewhere, CA'
const PROFILE_PICTURE_FILENAME = 'bar.jpeg'
const PROFILE_PICTURE_CONTENT = 'organization profile picture'
const PROFILE_PICTURE_MIMETYPE = 'image/jpeg'

test('organization settings', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.setPlan(Plan.team)
      api.setCurrentOrganization(api.defaultOrganization)
    },
  })
    .step('Verify initial organization state', (_, { api }) => {
      expect(api.defaultUser.isOrganizationAdmin).toBe(true)
      expect(api.currentOrganization()?.name).toBe(api.defaultOrganizationName)
      expect(api.currentOrganization()?.email).toBe(null)
      expect(api.currentOrganization()?.picture).toBe(null)
      expect(api.currentOrganization()?.website).toBe(null)
      expect(api.currentOrganization()?.address).toBe(null)
    })
    .goToPage.settings()
    .goToSettingsTab.organization()
    .organizationForm()
    .fillName(NEW_NAME)
    .do((_, context) => {
      context.calls = context.api.trackCalls()
    })
    .save()
    .step('Set organization name', (_, { api, calls }) => {
      expect(api.currentOrganization()?.name).toBe(NEW_NAME)
      expect(api.currentUser()?.name).not.toBe(NEW_NAME)
      expect(calls.updateOrganization).toMatchObject([{ name: NEW_NAME }])
    })
    .organizationForm()
    .fillName('')
    .do((_, context) => {
      context.calls = context.api.trackCalls()
    })
    .save()
    .step('Unsetting organization name should fail', (_, { api, calls }) => {
      expect(api.currentOrganization()?.name).toBe(NEW_NAME)
      expect(calls.updateOrganization).toMatchObject([{ name: '' }])
    })
    .organizationForm()
    .cancel()
    .organizationForm()
    .fillEmail(INVALID_EMAIL)
    .save()
    .step('Setting invalid email should fail', (_, { api }) => {
      expect(api.currentOrganization()?.email).toBe('')
    })
    .organizationForm()
    .fillEmail(NEW_EMAIL)
    .save()
    .step('Set email', (_, { api }) => {
      expect(api.currentOrganization()?.email).toBe(NEW_EMAIL)
    })
    .organizationForm()
    .fillWebsite(NEW_WEBSITE)
    .save()
    // NOTE: It is not yet possible to unset the website or the location.
    .step('Set website', async (_, { api }) => {
      expect(api.currentOrganization()?.website).toBe(NEW_WEBSITE)
    })
    .organizationForm()
    .fillLocation(NEW_LOCATION)
    .save()
    .step('Set website', async (_, { api }) => {
      expect(api.currentOrganization()?.address).toBe(NEW_LOCATION)
    }))

test('upload organization profile picture', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (theApi) => {
      theApi.setPlan(Plan.team)
    },
  })
    .goToPage.settings()
    .goToSettingsTab.organization()
    .uploadProfilePicture(
      PROFILE_PICTURE_FILENAME,
      PROFILE_PICTURE_CONTENT,
      PROFILE_PICTURE_MIMETYPE,
    )
    .step('Profile picture should be updated', async (_, { api }) => {
      await expect(() => {
        expect(api.currentOrganizationProfilePicture()).toEqual(PROFILE_PICTURE_CONTENT)
      }).toPass()
    }))
