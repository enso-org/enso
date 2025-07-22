/** @file Test the drive view. */
import { expect, test, type Page } from 'playwright/test'

import { COLORS } from 'enso-common/src/services/Backend'
import { mockAllAndLogin } from './actions'

const LABEL_NAME = 'aaaa'

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
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
    }))
