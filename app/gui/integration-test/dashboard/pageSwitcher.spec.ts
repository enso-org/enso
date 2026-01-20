/** @file Test the login flow. */
import { expect, test, type Page } from 'integration-test/base'

/** Find an editor container. */
function locateEditor(page: Page) {
  // Test ID of a placeholder editor component used during testing.
  return page.locator('.ProjectView')
}

/** Find a drive view. */
function locateDriveView(page: Page) {
  // This has no identifying features.
  return page.getByTestId('drive-view')
}

test.describe(() => {
  test.use({ featureFlags: { enableCloudExecution: true } })
  test('page switcher', async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()

      .newEmptyProject()
      .do(async (thePage) => {
        await expect(locateDriveView(thePage)).toBeHidden()
        await expect(locateEditor(thePage)).toBeVisible()
      })
      .do(async (thePage) => {
        await expect(locateDriveView(thePage)).toBeHidden()
        await expect(locateEditor(thePage)).toBeVisible()
      })
      .goToPage.drive()
      .do(async (thePage) => {
        await expect(locateDriveView(thePage)).toBeVisible()
        await expect(locateEditor(thePage)).toBeHidden()
      })
      .goToPage.projectView()
      .do(async (thePage) => {
        await expect(locateDriveView(thePage)).toBeHidden()
        await expect(locateEditor(thePage)).toBeVisible()
      })
  })
})
