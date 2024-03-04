/** @file Test copying, moving, cutting and pasting. */
import * as test from '@playwright/test'

import * as actions from './actions'

test.test.beforeEach(actions.mockAllAndLogin)

test.test('edit name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const mod = await actions.modModifier(page)
  const newName = 'foo bar baz'

  await actions.locateNewFolderIcon(page).click()
  await actions.locateAssetRowName(assetRows.nth(0)).click({ modifiers: [mod] })
  await actions.locateAssetRowName(assetRows.nth(0)).fill(newName)
  await actions.locateEditingTick(assetRows.nth(0)).click()
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + newName))
})

test.test('edit name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const newName = 'foo bar baz quux'

  await actions.locateNewFolderIcon(page).click()
  await actions.locateAssetRowName(assetRows.nth(0)).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(assetRows.nth(0)).fill(newName)
  await actions.locateAssetRowName(assetRows.nth(0)).press('Enter')
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + newName))
})

test.test('cancel editing name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const mod = await actions.modModifier(page)
  const newName = 'foo bar baz'

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(assetRows.nth(0)).textContent()) ?? ''
  await actions.locateAssetRowName(assetRows.nth(0)).click({ modifiers: [mod] })
  await actions.locateAssetRowName(assetRows.nth(0)).fill(newName)
  await actions.locateEditingCross(assetRows.nth(0)).click()
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + oldName))
})

test.test('cancel editing name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const newName = 'foo bar baz quux'

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(assetRows.nth(0)).textContent()) ?? ''
  await actions.locateAssetRowName(assetRows.nth(0)).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(assetRows.nth(0)).fill(newName)
  await actions.locateAssetRowName(assetRows.nth(0)).press('Escape')
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + oldName))
})

test.test('change to blank name', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)
  const mod = await actions.modModifier(page)

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(assetRows.nth(0)).textContent()) ?? ''
  await actions.locateAssetRowName(assetRows.nth(0)).click({ modifiers: [mod] })
  await actions.locateAssetRowName(assetRows.nth(0)).fill('')
  await actions.locateEditingTick(assetRows.nth(0)).click()
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + oldName))
})

test.test('change to blank name (keyboard)', async ({ page }) => {
  const assetRows = actions.locateAssetRows(page)

  await actions.locateNewFolderIcon(page).click()
  const oldName = (await actions.locateAssetRowName(assetRows.nth(0)).textContent()) ?? ''
  await actions.locateAssetRowName(assetRows.nth(0)).click()
  await actions.press(page, 'Mod+R')
  await actions.locateAssetRowName(assetRows.nth(0)).fill('')
  await actions.locateAssetRowName(assetRows.nth(0)).press('Enter')
  await test.expect(assetRows).toHaveCount(1)
  await test.expect(assetRows.nth(0)).toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(new RegExp('^' + oldName))
})
