/** @file Tests for the asset panel. */
import { expect, test, type Locator, type Page } from 'integration-test/base'

import { EmailAddress, UserId } from 'enso-common/src/services/Backend'

import { PermissionAction } from 'enso-common/src/utilities/permissions'

import { TEXT } from '../actions'

/** Find an asset panel. */
function locateRightPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('right-panel').locator('visible=true')
}

/** Find an asset description in an asset panel. */
function locateRightPanelDescription(page: Page) {
  // This has no identifying features.
  return locateRightPanel(page).getByTestId('asset-panel-description')
}

/** Find an asset description in an asset panel. */
function locateOwnerName(page: Page) {
  // This has no identifying features.
  return locateRightPanel(page).getByTestId('asset-panel-owner')
}

/** Find the contents of the Markdown editor within the given {@link Locator}. */
function locateMarkdownContent(locator: Locator) {
  return locator.getByTestId('documentation-editor-content')
}

/** An example description for the asset selected in the asset panel. */
const DESCRIPTION = 'foo bar'
/** A description written as part of the test */
const NEW_DESCRIPTION = 'Bar Baz'
/** An example owner username for the asset selected in the asset panel. */
const USERNAME = 'baz quux'
/** An example owner email for the asset selected in the asset panel. */
const EMAIL = 'baz.quux@email.com'

test('asset panel contents', async ({ drivePage, page, cloudApi }) => {
  const { defaultOrganizationId, defaultUserId } = cloudApi
  cloudApi.addProject({
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

  await drivePage.goToCategory
    .cloud()
    .driveTable.clickRow(0)
    .togglePropertiesAssetPanel()
    .do(async () => {
      await expect(locateOwnerName(page).getByText(USERNAME)).toBeVisible()
    })
    .togglePropertiesAssetPanel()
    .do(async () => {
      await expect(locateRightPanelDescription(page)).toBeHidden()
    })
})

test('Asset Panel Decription', async ({ drivePage, cloudApi, page }) => {
  cloudApi.addFile({
    title: 'File',
    description: DESCRIPTION,
  })

  await drivePage.goToCategory
    .cloud()
    .driveTable.clickRow(0)
    .toggleDescriptionAssetPanel()
    .do(async () => {
      const descriptionEditor = locateRightPanelDescription(page)
      await expect(descriptionEditor).toBeVisible()
      await expect(descriptionEditor).toContainText(DESCRIPTION)
      await descriptionEditor.click()
      await page.keyboard.insertText(NEW_DESCRIPTION)
    })
    .driveTable.clickAway()
    .do(() => expect(locateRightPanelDescription(page)).toBeHidden())
    .driveTable.clickRow(0)
    .do(async () => {
      await expect(locateRightPanelDescription(page)).toContainText(
        `${DESCRIPTION}${NEW_DESCRIPTION}`,
      )
    })
})

test('Asset Panel documentation view', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject({})
  await drivePage.goToCategory
    .cloud()
    .driveTable.clickRow(0)
    .toggleDocsAssetPanel()
    .withRightPanel(async (rightPanel) => {
      await expect(locateMarkdownContent(rightPanel)).toBeVisible()
      await expect(locateMarkdownContent(rightPanel)).toHaveText(/Project Goal/)
      await expect(rightPanel.getByText(TEXT.arbitraryFetchImageError)).toBeHidden()
    })
})

test('Assets Panel docs images', async ({ drivePage, cloudApi }) => {
  cloudApi.addProject({})
  await drivePage.goToCategory
    .cloud()
    .driveTable.clickRow(0)
    .toggleDocsAssetPanel()
    .withRightPanel(async (assetPanel) => {
      const content = locateMarkdownContent(assetPanel)
      await expect(content).toBeVisible()

      for (const image of await content.getByRole('img').all()) {
        await expect(image).toBeVisible()
        await expect(image).toHaveJSProperty('complete', true)
      }
    })
})
