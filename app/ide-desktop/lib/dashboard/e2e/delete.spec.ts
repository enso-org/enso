/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('delete and restore', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const contextMenu = actions.locateContextMenus(page)

  await actions.locateNewFolderIcon(page).click()
  await test.expect(assetRows).toHaveCount(1)

  await assetRows.nth(0).click({ button: 'right' })
  await actions.locateMoveToTrashButton(contextMenu).click()

  await actions.expectPlaceholderRow(page)

  await actions.locateTrashButton(page).click()
  await test.expect(assetRows).toHaveCount(1)

  await assetRows.nth(0).click({ button: 'right' })
  await actions.locateRestoreFromTrashButton(contextMenu).click()
  await actions.expectTrashPlaceholderRow(page)

  await actions.locateHomeButton(page).click()
  await test.expect(assetRows).toHaveCount(1)
})

test.test('delete and restore (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  await test.expect(assetRows).toHaveCount(1)

  await assetRows.nth(0).click()
  await actions.press(page, 'Delete')
  await actions.expectPlaceholderRow(page)

  await actions.locateTrashButton(page).click()
  await test.expect(assetRows).toHaveCount(1)

  await assetRows.nth(0).click()
  await actions.press(page, 'Mod+R')
  await actions.expectTrashPlaceholderRow(page)

  await actions.locateHomeButton(page).click()
  await test.expect(assetRows).toHaveCount(1)
})
