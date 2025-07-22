/** @file Tests for the asset panel. */
import { expect, test, type Locator, type Page } from 'playwright/test'

import { EmailAddress, UserId } from '#/services/Backend'

import { PermissionAction } from '#/utilities/permissions'

import { mockAllAndLogin, TEXT } from './actions'

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
    .togglePropertiesAssetPanel()
    .do(async () => {
      await expect(locateOwnerName(page).getByText(USERNAME)).toBeVisible()
    })
    .togglePropertiesAssetPanel()
    .do(async () => {
      await expect(locateRightPanelDescription(page)).not.toBeVisible()
    }))

test('Asset Panel Decription', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addFile({
        title: 'File',
        description: DESCRIPTION,
      })
    },
  })
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
    .do(() => expect(locateRightPanelDescription(page)).not.toBeVisible())
    .driveTable.clickRow(0)
    .do(async () => {
      await expect(locateRightPanelDescription(page)).toContainText(
        `${DESCRIPTION}${NEW_DESCRIPTION}`,
      )
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
    .withRightPanel(async (rightPanel) => {
      await expect(locateMarkdownContent(rightPanel)).toBeVisible()
      await expect(locateMarkdownContent(rightPanel)).toHaveText(/Project Goal/)
      await expect(rightPanel.getByText(TEXT.arbitraryFetchImageError)).not.toBeVisible()
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
    .withRightPanel(async (assetPanel) => {
      const content = locateMarkdownContent(assetPanel)
      await expect(content).toBeVisible()

      for (const image of await content.getByRole('img').all()) {
        await expect(image).toBeVisible()
        await expect(image).toHaveJSProperty('complete', true)
      }
    })
})
