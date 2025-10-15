/** @file Test the drive view. */
import { expect, test, type Page } from 'integration-test/base'

import { COLORS } from 'enso-common/src/services/Backend'

const LABEL_NAME = 'aaaa'

/** Find the context menu. */
function locateContextMenu(page: Page) {
  // This has no identifying features.
  return page.getByTestId('context-menu')
}

test.describe(() => {
  test.use({
    setupApi: {
      cloud: (cloudApi) => {
        cloudApi.addLabel(LABEL_NAME, COLORS[0])
      },
    },
  })
  test('drive view', async ({ drivePage }) => {
    await drivePage.goToCategory
      .cloud()
      .driveTable.expectPlaceholderRow()
      .withDriveView(async (view) => {
        await view.click({ button: 'right' })
      })
      .do(async (page) => {
        await expect(locateContextMenu(page)).toHaveCount(1)
      })
      .press('Escape')
      .do(async (thePage) => {
        await expect(locateContextMenu(thePage)).toHaveCount(0)
      })
  })
})
