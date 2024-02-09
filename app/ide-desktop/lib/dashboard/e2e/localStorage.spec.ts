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
    .toHaveText(['Name', 'Modified', 'Shared with'])
  await actions.locateLabelsColumnToggle(page).click()
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels'])
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Labels'])
  await actions.locateDocsColumnToggle(page).click()
  await actions.locateLabelsColumnToggle(page).click()
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Docs'])
  await actions.login({ page })
  await test
    .expect(actions.locateAssetsColumnHeaders(page))
    .toHaveText(['Name', 'Modified', 'Shared with', 'Docs'])
  await actions.locateLabelsColumnToggle(page).click()
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
    .toHaveText(['Name', 'Modified', 'Shared with'])
})

test.test('backend type', async ({ page }) => {
  await actions.expectBackgroundFrameSelected(actions.locateCloudBackendButton(page))
  await actions.expectBackgroundFrame(actions.locateLocalBackendButton(page))
  await actions.locateLocalBackendButton(page).click()
  await actions.expectBackgroundFrame(actions.locateCloudBackendButton(page))
  await actions.expectBackgroundFrameSelected(actions.locateLocalBackendButton(page))
  await actions.login({ page })
  await actions.expectBackgroundFrame(actions.locateCloudBackendButton(page))
  await actions.expectBackgroundFrameSelected(actions.locateLocalBackendButton(page))
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ backendType: 'invalid' }))
  })
  await actions.login({ page })
  await actions.expectBackgroundFrameSelected(actions.locateCloudBackendButton(page))
  await actions.expectBackgroundFrame(actions.locateLocalBackendButton(page))
})

test.test('drive category', async ({ page }) => {
  await actions.expectNoBackground(actions.locateRecentCategory(page))
  await actions.expectBackgroundFrameSelected(actions.locateHomeCategory(page))
  await actions.expectNoBackground(actions.locateTrashCategory(page))
  await actions.locateRecentCategory(page).click()
  await actions.expectBackgroundFrameSelected(actions.locateRecentCategory(page))
  await actions.login({ page })
  await actions.expectBackgroundFrameSelected(actions.locateRecentCategory(page))
  await actions.locateTrashCategory(page).click()
  await actions.expectBackgroundFrameSelected(actions.locateTrashCategory(page))
  await actions.login({ page })
  await actions.expectBackgroundFrameSelected(actions.locateTrashCategory(page))
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ driveCategory: 'invalid' }))
  })
  await actions.login({ page })
  await actions.expectBackgroundFrameSelected(actions.locateHomeCategory(page))
})

test.test('asset panel visibility', async ({ page }) => {
  await actions.locateNewFolderIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).not.toBeVisible()
  await actions.locateAssetRows(page).first().click()
  await actions.locateAssetPanelIcon(page).click()
  await test.expect(actions.locateAssetPanel(page)).toBeVisible()
  await actions.login({ page })
  await actions.locateAssetRows(page).first().click()
  await test.expect(actions.locateAssetPanel(page)).toBeVisible()
  // Invalid `localStorage` values should reset the setting rather than crashing
  // the application.
  await page.evaluate(() => {
    localStorage.setItem('enso', JSON.stringify({ isAssetPanelVisible: 'invalid' }))
  })
  await actions.login({ page })
  await actions.locateAssetRows(page).first().click()
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
