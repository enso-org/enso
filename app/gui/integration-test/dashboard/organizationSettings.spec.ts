/** @file Test the organization settings tab. */
import { expect, test } from 'integration-test/base'

import { Plan } from 'enso-common/src/services/Backend'

const NEW_NAME = 'another organization-name'
const INVALID_EMAIL = 'invalid@email'
const NEW_EMAIL = 'organization@email.com'
const NEW_WEBSITE = 'organization.org'
const NEW_LOCATION = 'Somewhere, CA'
const PROFILE_PICTURE_FILENAME = 'bar.jpeg'
const PROFILE_PICTURE_CONTENT = 'organization profile picture'
const PROFILE_PICTURE_MIMETYPE = 'image/jpeg'

test.describe(() => {
  test.use({
    setupApi: {
      cloud: (cloudApi) => {
        cloudApi.setPlan(Plan.team)
      },
    },
  })
  test('organization settings', async ({ drivePage, cloudApi }) => {
    let calls = cloudApi.trackCalls()
    await drivePage.goToCategory
      .cloud()
      .step('Verify initial organization state', () => {
        expect(cloudApi.defaultUser.isOrganizationAdmin).toBe(true)
        expect(cloudApi.currentOrganization()?.name).toBe(cloudApi.defaultOrganizationName)
        expect(cloudApi.currentOrganization()?.email).toBe(null)
        expect(cloudApi.currentOrganization()?.picture).toBe(null)
        expect(cloudApi.currentOrganization()?.website).toBe(null)
        expect(cloudApi.currentOrganization()?.address).toBe(null)
      })
      .goToPage.settings()
      .goToSettingsTab.organization()
      .organizationForm()
      .fillName(NEW_NAME)
      .do(() => {
        calls = cloudApi.trackCalls()
      })
      .save()
      .step('Set organization name', () => {
        expect(cloudApi.currentOrganization()?.name).toBe(NEW_NAME)
        expect(cloudApi.currentUser()?.name).not.toBe(NEW_NAME)
        expect(calls.updateOrganization).toMatchObject([{ name: NEW_NAME }])
      })
      .organizationForm()
      .fillName('')
      .do(() => {
        calls = cloudApi.trackCalls()
      })
      .save(false)
      .step('Unsetting organization name should fail', () => {
        expect(cloudApi.currentOrganization()?.name).toBe(NEW_NAME)
        expect(calls.updateOrganization).toMatchObject([{ name: '' }])
      })
      .organizationForm()
      .cancel()
      .organizationForm()
      .fillEmail(INVALID_EMAIL)
      .save(false)
      .step('Setting invalid email should fail', () =>
        expect(cloudApi.currentOrganization()?.email).toBe(''),
      )
      .organizationForm()
      .fillEmail(NEW_EMAIL)
      .save(true)
      .step('Set email', () => expect(cloudApi.currentOrganization()?.email).toBe(NEW_EMAIL))
      .organizationForm()
      .fillWebsite(NEW_WEBSITE)
      .save(true)
      // NOTE: It is not yet possible to unset the website or the location.
      .step('Set website', () => expect(cloudApi.currentOrganization()?.website).toBe(NEW_WEBSITE))
      .organizationForm()
      .fillLocation(NEW_LOCATION)
      .save(true)
      .step('Set website', () => expect(cloudApi.currentOrganization()?.address).toBe(NEW_LOCATION))
  })

  test('upload organization profile picture', async ({ drivePage, cloudApi }) => {
    await drivePage.goToCategory
      .cloud()
      .goToPage.settings()
      .goToSettingsTab.organization()
      .uploadProfilePicture(
        PROFILE_PICTURE_FILENAME,
        PROFILE_PICTURE_CONTENT,
        PROFILE_PICTURE_MIMETYPE,
      )
      .step('Profile picture should be updated', () =>
        expect(cloudApi.currentOrganizationProfilePicture()).toEqual(PROFILE_PICTURE_CONTENT),
      )
  })
})
