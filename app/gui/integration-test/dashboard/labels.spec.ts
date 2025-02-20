/** @file Test dragging of labels. */
import { expect, test, type Locator, type Page } from '@playwright/test'

import { COLORS } from '#/services/Backend'

import { mockAllAndLogin } from './actions'

const LABEL = 'aaaa'
const ASSET_ROW_SAFE_POSITION = { x: 300, y: 16 }

/** Click an asset row. The center must not be clicked as that is the button for adding a label. */
async function clickAssetRow(assetRow: Locator) {
  await assetRow.click({ position: ASSET_ROW_SAFE_POSITION })
}

/** Find labels in the "Labels" column of the assets table. */
function locateAssetLabels(page: Locator) {
  return page.getByTestId('asset-label')
}

/** Find a labels panel. */
function locateLabelsPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('labels')
}

/** Find all labels in the labels panel. */
function locateLabelsPanelLabels(page: Page, name?: string) {
  return (
    locateLabelsPanel(page)
      .getByRole('button')
      .filter(name != null ? { has: page.getByText(name) } : {})
      // The delete button is also a `button`.
      .and(page.locator(':nth-child(1)'))
  )
}

test('drag labels onto single row', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel(LABEL, COLORS[0])
      api.addLabel('bbbb', COLORS[1])
      api.addLabel('cccc', COLORS[2])
      api.addLabel('dddd', COLORS[3])
      api.addDirectory({ title: 'foo' })
      api.addSecret({ title: 'bar' })
      api.addFile({ title: 'baz' })
      api.addSecret({ title: 'quux' })
    },
  }).driveTable.withRows(async (rows, _, _context, page) => {
    const labelEl = locateLabelsPanelLabels(page, LABEL)
    await expect(labelEl).toBeVisible()
    await labelEl.dragTo(rows.nth(1))
    await expect(locateAssetLabels(rows.nth(0)).getByText(LABEL)).not.toBeVisible()
    await expect(locateAssetLabels(rows.nth(1)).getByText(LABEL)).toBeVisible()
    await expect(locateAssetLabels(rows.nth(2)).getByText(LABEL)).not.toBeVisible()
    await expect(locateAssetLabels(rows.nth(3)).getByText(LABEL)).not.toBeVisible()
  }))

test('drag labels onto multiple rows', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel(LABEL, COLORS[0])
      api.addLabel('bbbb', COLORS[1])
      api.addLabel('cccc', COLORS[2])
      api.addLabel('dddd', COLORS[3])
      api.addDirectory({ title: 'foo' })
      api.addSecret({ title: 'bar' })
      api.addFile({ title: 'baz' })
      api.addSecret({ title: 'quux' })
    },
  })
    .withModPressed((self) =>
      self.driveTable.withRows(async (rows, _, _context, page) => {
        const labelEl = locateLabelsPanelLabels(page, LABEL)
        await expect(rows).toHaveCount(4)
        await clickAssetRow(rows.nth(0))
        await clickAssetRow(rows.nth(2))
        await expect(labelEl).toBeVisible()
        await labelEl.dragTo(rows.nth(2))
      }),
    )
    .driveTable.withRows(async (rows) => {
      await expect(locateAssetLabels(rows.nth(0)).getByText(LABEL)).toBeVisible()
      await expect(locateAssetLabels(rows.nth(1)).getByText(LABEL)).not.toBeVisible()
      await expect(locateAssetLabels(rows.nth(2)).getByText(LABEL)).toBeVisible()
      await expect(locateAssetLabels(rows.nth(3)).getByText(LABEL)).not.toBeVisible()
    }))
