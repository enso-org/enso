/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('drive view', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  // Drive view
  await test.expect(actions.locateDriveView(page)).toBeVisible()
  await actions.expectPlaceholderRow(page)
  // Assets table with one asset
  await actions.locateNewProjectButton(page).click()
  // The placeholder row becomes hidden.
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(actions.locateAssetsTable(page)).toBeVisible()
  await actions.locateDrivePageIcon(page).click()
  await actions.locateNewProjectButton(page).click()
  await test.expect(assetRows).toHaveCount(2)
  await actions.locateDrivePageIcon(page).click()
  // The last opened project needs to be stopped, to remove the toast notification notifying the
  // user that project creation may take a while. Previously opened projects are stopped when the
  // new project is created.
  await actions.locateStopProjectButton(assetRows.nth(0)).click()
  // Project context menu
  await assetRows.nth(0).click({ button: 'right' })
  const contextMenu = actions.locateContextMenus(page)
  await test.expect(contextMenu).toBeVisible()
  await actions.locateMoveToTrashButton(contextMenu).click()
  await test.expect(assetRows).toHaveCount(1)
})
