/** @file Test that `LocalStorage` is restoring saved configuration, and deleting invalid
 * configuration. */
import * as test from '@playwright/test'

import * as actions from './actions'

// Tests for `loginRedirect` and `project` omitted for now as they are more
// difficult to test.
// Tests for `assetPanelTab` also omitted for now.

test.test.beforeEach(actions.mockAllAndLogin)

test.test('extra columns', async ({ page }) => {
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels'])
  await actions.locateLabelsColumnToggle(page).click()
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with'])
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with'])
  await actions.locateDocsColumnToggle(page).click()
  await actions.locateLabelsColumnToggle(page).click()
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels', 'Docs'])
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels', 'Docs'])
  await actions.locateAccessedByProjectsColumnToggle(page).click()
  await actions.locateAccessedDataColumnToggle(page).click()
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText([
      'Name',
      'Modified',
      'Shared with',
      'Labels',
      'Accessed by projects',
      'Accessed data',
      'Docs',
    ])
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText([
      'Name',
      'Modified',
      'Shared with',
      'Labels',
      'Accessed by projects',
      'Accessed data',
      'Docs',
    ])
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ extraColumns: 'invalid' }))
  })
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels'])
})

test.test('backend type', async ({ page }) => {
  await test.expect(page.getByText('Cloud Drive')).toBeVisible()
  await test.expect(page.getByText('Local Drive')).not.toBeVisible()
  await actions.locateLocalBackendButton(page).click()
  await test.expect(page.getByText('Cloud Drive')).not.toBeVisible()
  await test.expect(page.getByText('Local Drive')).toBeVisible()
  await actions.login({ page })
  await test.expect(page.getByText('Cloud Drive')).not.toBeVisible()
  await test.expect(page.getByText('Local Drive')).toBeVisible()
  await actions.locateCloudBackendButton(page).click()
  await test.expect(page.getByText('Cloud Drive')).toBeVisible()
  await test.expect(page.getByText('Local Drive')).not.toBeVisible()
  await actions.login({ page })
  await test.expect(page.getByText('Cloud Drive')).toBeVisible()
  await test.expect(page.getByText('Local Drive')).not.toBeVisible()
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ backendType: 'invalid' }))
  })
  await actions.login({ page })
  await test.expect(page.getByText('Cloud Drive')).toBeVisible()
  await test.expect(page.getByText('Local Drive')).not.toBeVisible()
})

test.test('drive category', async ({ page }) => {
  await actions.expectNotClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateTrashCategory(page).locator('> div'))
  await actions.locateRecentCategory(page).click()
  await actions.expectClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateTrashCategory(page).locator('> div'))
  await actions.login({ page })
  await actions.expectClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateTrashCategory(page).locator('> div'))
  await actions.locateTrashCategory(page).click()
  await actions.expectNotClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectClassActive(actions.locateTrashCategory(page).locator('> div'))
  await actions.login({ page })
  await actions.expectNotClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectClassActive(actions.locateTrashCategory(page).locator('> div'))
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ driveCategory: 'invalid' }))
  })
  await actions.login({ page })
  await actions.expectNotClassActive(actions.locateRecentCategory(page).locator('> div'))
  await actions.expectClassActive(actions.locateHomeCategory(page).locator('> div'))
  await actions.expectNotClassActive(actions.locateTrashCategory(page).locator('> div'))
})

test.test('asset panel visibility', async ({ page }) => {
  await actions.locateNewFolderIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).not.toBeVisible()
  await actions.locateAssetPanelIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).toBeVisible()
  await actions.login({ page })
  await test.expect(actions.locateAssetPanel(page)).toBeVisible()
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ isAssetPanelVisible: 'invalid' }))
  })
  await actions.login({ page })
  await test.expect(actions.locateAssetsTable(page)).toBeVisible()
  await test.expect(actions.locateAssetPanel(page)).not.toBeVisible()
})

test.test('page', async ({ page }) => {
  await test.expect(actions.locateAssetsTable(page)).toBeVisible()
  await actions.locateHomePageIcon(page).click()
  await test.expect(actions.locateSamplesList(page)).toBeVisible()
  await actions.login({ page })
  await test.expect(actions.locateSamplesList(page)).toBeVisible()
  await actions.locateDrivePageIcon(page).click()
  await actions.locateNewProjectButton(page).click()
  await actions.mockIDEContainer({ page })
  await test.expect(actions.locateEditor(page)).toBeVisible()
  await actions.login({ page })
  await actions.mockIDEContainer({ page })
  await test.expect(actions.locateEditor(page)).toBeVisible()
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ page: 'invalid' }))
  })
  await actions.login({ page })
  await test.expect(actions.locateAssetsTable(page)).toBeVisible()
})
