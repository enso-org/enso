/** @file Test the drive view. */
import * as test from '@playwright/test'

import * as actions from './actions'
import * as api from './api'

test.test('drive view', async ({ page }) => {
  await api.mockApi(page)
  await actions.mockDate(page)
  await actions.login(page)

  // Screenshot #1: Drive view
  // Initially, the table contains the header row and the placeholder row.
  await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(2)
  await test.expect(actions.locateDriveView(page)).toHaveScreenshot()

  // Screenshot #2: Assets table with one asset
  await actions.locateNewProjectButton(page).click()
  // The placeholder row becomes hidden.
  await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(2)
  await test.expect(actions.locateAssetsTable(page)).toHaveScreenshot()

  await actions.locateNewProjectButton(page).click()
  await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(3)

  // These are guarded by the `not.toBeUndefined` below.
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const firstAssetRow = (await actions.locateAssetsTableRows(page).all())[1]!
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const secondAssetRow = (await actions.locateAssetsTableRows(page).all())[2]!
  test.expect(firstAssetRow).not.toBeUndefined()
  test.expect(secondAssetRow).not.toBeUndefined()
  // The last opened project needs to be stopped, to remove the toast notification notifying the
  // user that project creation may take a while. Previously opened projects are stopped when the
  // new project is created.
  await actions.locateStopProjectButton(secondAssetRow).click()

  // Screenshot #3: Project context menu
  await firstAssetRow.click({ button: 'right' })
  const contextMenu = actions.locateContextMenus(page)
  await test.expect(contextMenu).toHaveScreenshot()

  await actions.locateMoveToTrashButton(contextMenu).click()
  await test.expect(actions.locateAssetsTableRows(page)).toHaveCount(2)
})
