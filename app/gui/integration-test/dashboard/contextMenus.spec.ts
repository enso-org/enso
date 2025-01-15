/** @file Test the drive view. */
import { expect, test, type Page } from '@playwright/test'

import { COLORS } from 'enso-common/src/services/Backend'
import { mockAllAndLogin } from './actions'

const LABEL_NAME = 'aaaa'

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

/** Find labels in the "Labels" column of the assets table. */
function locateAssetLabels(page: Page) {
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

test('drive view', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      api.addLabel(LABEL_NAME, COLORS[0])
    },
  })
    .driveTable.expectPlaceholderRow()
    .withDriveView(async (view) => {
      await view.click({ button: 'right' })
    })
    .do(async (thePage) => {
      await expect(locateContextMenu(thePage)).toHaveCount(1)
    })
    .press('Escape')
    .do(async (thePage) => {
      await expect(locateContextMenu(thePage)).toHaveCount(0)
    })
    .createFolder()
    .driveTable.withRows(async (rows, _, _context, thePage) => {
      await locateLabelsPanelLabels(thePage, LABEL_NAME).dragTo(rows.nth(0))
      await locateAssetLabels(thePage).first().click({ button: 'right' })
      await expect(locateContextMenu(thePage)).toHaveCount(1)
    })
    .press('Escape')
    .do(async (thePage) => {
      await expect(locateContextMenu(thePage)).toHaveCount(0)
    }))
