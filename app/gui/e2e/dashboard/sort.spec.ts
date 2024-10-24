/** @file Test sorting of assets columns. */
import * as test from '@playwright/test'

import * as dateTime from '#/utilities/dateTime'

import * as actions from './actions'

// =================
// === Constants ===
// =================

const START_DATE_EPOCH_MS = 1.7e12
/** The number of milliseconds in a minute. */
const MIN_MS = 60_000

// =============
// === Tests ===
// =============

test.test('sort', async ({ page }) => {
  await actions.mockAll({
    page,
    setupAPI: (api) => {
      const date1 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS))
      const date2 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 1 * MIN_MS))
      const date3 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 2 * MIN_MS))
      const date4 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 3 * MIN_MS))
      const date5 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 4 * MIN_MS))
      const date6 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 5 * MIN_MS))
      const date7 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 6 * MIN_MS))
      const date8 = dateTime.toRfc3339(new Date(START_DATE_EPOCH_MS + 7 * MIN_MS))
      api.addDirectory('a directory', { modifiedAt: date4 })
      api.addDirectory('G directory', { modifiedAt: date6 })
      api.addProject('C project', { modifiedAt: date7 })
      api.addSecret('H secret', { modifiedAt: date2 })
      api.addProject('b project', { modifiedAt: date1 })
      api.addFile('d file', { modifiedAt: date8 })
      api.addSecret('f secret', { modifiedAt: date3 })
      api.addFile('e file', { modifiedAt: date5 })
      // By date:
      // b project
      // h secret
      // f secret
      // a directory
      // e file
      // g directory
      // c project
      // d file
    },
  })
  const assetRows = actions.locateAssetRows(page)
  const nameHeading = actions.locateNameColumnHeading(page)
  const modifiedHeading = actions.locateModifiedColumnHeading(page)
  await actions.login({ page })

  // By default, assets should be grouped by type.
  // Assets in each group are ordered by insertion order.
  await actions.expectOpacity0(actions.locateSortAscendingIcon(nameHeading))
  await test.expect(actions.locateSortDescendingIcon(nameHeading)).not.toBeVisible()
  await actions.expectOpacity0(actions.locateSortAscendingIcon(modifiedHeading))
  await test.expect(actions.locateSortDescendingIcon(modifiedHeading)).not.toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(1)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(2)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(3)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(4)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(6)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(7)).toHaveText(/^f secret/)

  // Sort by name ascending.
  await nameHeading.click()
  await actions.expectNotOpacity0(actions.locateSortAscendingIcon(nameHeading))
  await test.expect(assetRows.nth(0)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(1)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(2)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(3)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(4)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^f secret/)
  await test.expect(assetRows.nth(6)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(7)).toHaveText(/^H secret/)

  // Sort by name descending.
  await nameHeading.click()
  await actions.expectNotOpacity0(actions.locateSortDescendingIcon(nameHeading))
  await test.expect(assetRows.nth(0)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(1)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(2)).toHaveText(/^f secret/)
  await test.expect(assetRows.nth(3)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(4)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(6)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(7)).toHaveText(/^a directory/)

  // Sorting should be unset.
  await nameHeading.click()
  await page.mouse.move(0, 0)
  await actions.expectOpacity0(actions.locateSortAscendingIcon(nameHeading))
  await test.expect(actions.locateSortDescendingIcon(nameHeading)).not.toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(1)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(2)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(3)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(4)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(6)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(7)).toHaveText(/^f secret/)

  // Sort by date ascending.
  await modifiedHeading.click()
  await actions.expectNotOpacity0(actions.locateSortAscendingIcon(modifiedHeading))
  await test.expect(assetRows.nth(0)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(1)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(2)).toHaveText(/^f secret/)
  await test.expect(assetRows.nth(3)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(4)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(6)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(7)).toHaveText(/^d file/)

  // Sort by date descending.
  await modifiedHeading.click()
  await actions.expectNotOpacity0(actions.locateSortDescendingIcon(modifiedHeading))
  await test.expect(assetRows.nth(0)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(1)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(2)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(3)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(4)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(5)).toHaveText(/^f secret/)
  await test.expect(assetRows.nth(6)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(7)).toHaveText(/^b project/)

  // Sorting should be unset.
  await modifiedHeading.click()
  await page.mouse.move(0, 0)
  await actions.expectOpacity0(actions.locateSortAscendingIcon(modifiedHeading))
  await test.expect(actions.locateSortDescendingIcon(modifiedHeading)).not.toBeVisible()
  await test.expect(assetRows.nth(0)).toHaveText(/^a directory/)
  await test.expect(assetRows.nth(1)).toHaveText(/^G directory/)
  await test.expect(assetRows.nth(2)).toHaveText(/^C project/)
  await test.expect(assetRows.nth(3)).toHaveText(/^b project/)
  await test.expect(assetRows.nth(4)).toHaveText(/^d file/)
  await test.expect(assetRows.nth(5)).toHaveText(/^e file/)
  await test.expect(assetRows.nth(6)).toHaveText(/^H secret/)
  await test.expect(assetRows.nth(7)).toHaveText(/^f secret/)
})
