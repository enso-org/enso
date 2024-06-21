/** @file Test dragging of labels. */
import * as test from '@playwright/test'

import * as backend from '#/services/Backend'

import * as actions from './actions'

test.test('drag labels onto single row', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const label = 'aaaa'
  api.addLabel(label, backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('bbbb', backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  api.addDirectory('foo')
  api.addSecret('bar')
  api.addFile('baz')
  api.addSecret('quux')
  await actions.reload({ page })

  await actions.locateLabelsPanelLabels(page, label).dragTo(assetRows.nth(1))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})

test.test('drag labels onto multiple rows', async ({ page }) => {
  const { api } = await actions.mockAllAndLogin({ page })
  const assetRows = actions.locateAssetRows(page)
  const labels = actions.locateLabelsPanelLabels(page)
  const label = 'aaaa'
  api.addLabel(label, backend.COLORS[0])
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('bbbb', backend.COLORS[1]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('cccc', backend.COLORS[2]!)
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  api.addLabel('dddd', backend.COLORS[3]!)
  api.addDirectory('foo')
  api.addSecret('bar')
  api.addFile('baz')
  api.addSecret('quux')
  await actions.reload({ page })

  await page.keyboard.down(await actions.modModifier(page))
  await actions.clickAssetRow(assetRows.nth(0))
  await actions.clickAssetRow(assetRows.nth(2))
  await labels.nth(0).dragTo(assetRows.nth(2))
  await page.keyboard.up(await actions.modModifier(page))
  await test.expect(actions.locateAssetLabels(assetRows.nth(0)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(1)).getByText(label)).not.toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(2)).getByText(label)).toBeVisible()
  await test.expect(actions.locateAssetLabels(assetRows.nth(3)).getByText(label)).not.toBeVisible()
})
