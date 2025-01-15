/** @file Tests for the asset panel. */
import { expect, test, type Page } from '@playwright/test'

import { EmailAddress, UserId } from '#/services/Backend'

import { PermissionAction } from '#/utilities/permissions'

import { mockAllAndLogin, TEXT } from './actions'

/** Find an asset panel. */
function locateAssetPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('asset-panel').locator('visible=true')
}

/** Find an asset description in an asset panel. */
function locateAssetPanelDescription(page: Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-description')
}

/** Find asset permissions in an asset panel. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
function locateAssetPanelPermissions(page: Page) {
  // This has no identifying features.
  return locateAssetPanel(page).getByTestId('asset-panel-permissions').getByRole('button')
}

/** An example description for the asset selected in the asset panel. */
const DESCRIPTION = 'foo bar'
/** An example owner username for the asset selected in the asset panel. */
const USERNAME = 'baz quux'
/** An example owner email for the asset selected in the asset panel. */
const EMAIL = 'baz.quux@email.com'

test('open and close asset panel', ({ page }) =>
  mockAllAndLogin({ page })
    .withAssetPanel(async (assetPanel) => {
      await expect(assetPanel).toBeVisible()
    })
    .toggleAssetPanel()
    .withAssetPanel(async (assetPanel) => {
      await expect(assetPanel).not.toBeVisible()
    }))

test('asset panel contents', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      const { defaultOrganizationId, defaultUserId } = api
      api.addProject({
        description: DESCRIPTION,
        permissions: [
          {
            permission: PermissionAction.own,
            user: {
              organizationId: defaultOrganizationId,
              // Using the default ID causes the asset to have a dynamic username.
              userId: UserId(defaultUserId + '2'),
              name: USERNAME,
              email: EmailAddress(EMAIL),
            },
          },
        ],
      })
    },
  })
    .driveTable.clickRow(0)
    .toggleDescriptionAssetPanel()
    .do(async () => {
      await expect(locateAssetPanelDescription(page)).toHaveText(DESCRIPTION)
      // `getByText` is required so that this assertion works if there are multiple permissions.
      // This is not visible; "Shared with" should only be visible on the Enterprise plan.
      // await expect(locateAssetPanelPermissions(page).getByText(USERNAME)).toBeVisible()
    }))

test('Asset Panel documentation view', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({})
    },
  })
    .driveTable.clickRow(0)
    .toggleDocsAssetPanel()
    .withAssetPanel(async (assetPanel) => {
      await expect(assetPanel.getByTestId('asset-panel-tab-panel-docs')).toBeVisible()
      await expect(assetPanel.getByTestId('asset-docs-content')).toBeVisible()
      await expect(assetPanel.getByTestId('asset-docs-content')).toHaveText(/Project Goal/)
      await expect(assetPanel.getByText(TEXT.arbitraryFetchImageError)).not.toBeVisible()
    }))

test('Assets Panel docs images', ({ page }) => {
  return mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addProject({})
    },
  })
    .do(() => {})
    .driveTable.clickRow(0)
    .toggleDocsAssetPanel()
    .withAssetPanel(async (assetPanel) => {
      await expect(assetPanel.getByTestId('asset-docs-content')).toBeVisible()

      for (const image of await assetPanel.getByRole('img').all()) {
        await expect(image).toBeVisible()
        await expect(image).toHaveJSProperty('complete', true)
      }
    })
})
