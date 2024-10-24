/** @file Test dragging of labels. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as actions from './actions'

export const ASSET_ROW_SAFE_POSITION = { x: 300, y: 16 }

/** Click an asset row. The center must not be clicked as that is the button for adding a label. */
export async function clickAssetRow(assetRow: test.Locator) {
  await assetRow.click({ position: ASSET_ROW_SAFE_POSITION })
}

test.test('drag labels onto single row', async ({ page }) => {
  const label = 'aaaa'
  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel(label, backend.COLORS[0])
      api.addLabel('bbbb', backend.COLORS[1])
      api.addLabel('cccc', backend.COLORS[2])
      api.addLabel('dddd', backend.COLORS[3])
      api.addDirectory('foo')
      api.addSecret('bar')
      api.addFile('baz')
      api.addSecret('quux')
    },
  })
  const assetRows = actions.locateAssetRows(page)
  const labelEl = actions.locateLabelsPanelLabels(page, label)
  await actions.relog({ page })

  await test.expect(labelEl).toBeVisible()
  await labelEl.dragTo(assetRows.nth(1))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})

test.test('drag labels onto multiple rows', async ({ page }) => {
  const label = 'aaaa'
  await actions.mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel(label, backend.COLORS[0])
      api.addLabel('bbbb', backend.COLORS[1])
      api.addLabel('cccc', backend.COLORS[2])
      api.addLabel('dddd', backend.COLORS[3])
      api.addDirectory('foo')
      api.addSecret('bar')
      api.addFile('baz')
      api.addSecret('quux')
    },
  })
  const assetRows = actions.locateAssetRows(page)
  const labelEl = actions.locateLabelsPanelLabels(page, label)

  await page.keyboard.down(await actions.modModifier(page))
  await test.expect(assetRows).toHaveCount(4)
  await clickAssetRow(assetRows.nth(0))
  await clickAssetRow(assetRows.nth(2))
  await test.expect(labelEl).toBeVisible()
  await labelEl.dragTo(assetRows.nth(2))
  await page.keyboard.up(await actions.modModifier(page))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})
