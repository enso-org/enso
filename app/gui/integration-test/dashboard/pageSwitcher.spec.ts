/** @file Test the login flow. */
import { expect, test, type Page } from 'integration-test/base'

/** Find an editor container. */
function locateEditor(page: Page) {
  // Test ID of a placeholder editor component used during testing.
  return page.locator('.ProjectView')
}

/** Find a drive view. */
function locateSettings(page: Page) {
  // This has no identifying features.
  return page.getByTestId('settings-panel')
}

test.describe(() => {
  test.use({ featureFlags: { enableCloudExecution: true } })
  test('page switcher', async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()
      .newEmptyProject()
      .do(async (thePage) => {
        await expect(locateSettings(thePage)).toBeHidden()
        await expect(locateEditor(thePage)).toBeVisible()
      })
      .goToPage.settings()
      .do(async (thePage) => {
        await expect(locateSettings(thePage)).toBeVisible()
        await expect(locateEditor(thePage)).toBeHidden()
      })
      .goToPage.projectView()
      .do(async (thePage) => {
        await expect(locateSettings(thePage)).toBeHidden()
        await expect(locateEditor(thePage)).toBeVisible()
      })
  })
})
