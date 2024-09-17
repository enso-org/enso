/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(({ page }) => actions.mockAllAndLogin({ page }))

test.test('edit name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)
  const newName = 'foo bar baz'

  await actions.locateNewFolderIcon(page).click()
  await actions.locateAssetRowName(row).click()
  await actions.locateAssetRowName(row).click()
  await actions.locateAssetRowName(row).fill(newName)
  await actions.locateEditingTick(row).click()
  await test.expect(row).toHaveText(new RegExp('^' + newName))
})

test.test('edit name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)
  const newName = 'foo bar baz quux'

  await actions.locateNewFolderIcon(page).click()
  await actions.locateAssetRowName(row).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(row).fill(newName)
  await actions.locateAssetRowName(row).press('Enter')
  await test.expect(row).toHaveText(new RegExp('^' + newName))
})

test.test('cancel editing name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)
  const newName = 'foo bar baz'

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(row).textContent()) ?? ''
  await actions.locateAssetRowName(row).click()
  await actions.locateAssetRowName(row).click()

  await actions.locateAssetRowName(row).fill(newName)
  await actions.locateEditingCross(row).click()
  await test.expect(row).toHaveText(new RegExp('^' + oldName))
})

test.test('cancel editing name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)
  const newName = 'foo bar baz quux'

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(row).textContent()) ?? ''
  await actions.locateAssetRowName(row).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(row).fill(newName)
  await actions.locateAssetRowName(row).press('Escape')
  await test.expect(row).toHaveText(new RegExp('^' + oldName))
})

test.test('change to blank name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(row).textContent()) ?? ''
  await actions.locateAssetRowName(row).click()
  await actions.locateAssetRowName(row).click()
  await actions.locateAssetRowName(row).fill('')
  await test.expect(actions.locateEditingTick(row)).not.toBeVisible()
  await actions.locateEditingCross(row).click()
  await test.expect(row).toHaveText(new RegExp('^' + oldName))
})

test.test('change to blank name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const row = assetRows.nth(0)

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(row).textContent()) ?? ''
  await actions.locateAssetRowName(row).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(row).fill('')
  await actions.locateAssetRowName(row).press('Enter')
  await test.expect(row).toHaveText(new RegExp('^' + oldName))
})
