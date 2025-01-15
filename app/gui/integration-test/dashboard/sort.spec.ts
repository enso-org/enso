/** @file Test sorting of assets columns. */
import { expect, test, type Locator } from '@playwright/test'

import { toRfc3339 } from '#/utilities/dateTime'

import { mockAllAndLogin } from './actions'

/** A test assertion to confirm that the element is fully transparent. */
async function expectOpacity0(locator: Locator) {
  await test.step('Expect `opacity: 0`', async () => {
    await expect(async () => {
      expect(await locator.evaluate((el) => getComputedStyle(el).opacity)).toBe('0')
    }).toPass()
  })
}

/** A test assertion to confirm that the element is not fully transparent. */
async function expectNotOpacity0(locator: Locator) {
  await test.step('Expect not `opacity: 0`', async () => {
    await expect(async () => {
      expect(await locator.evaluate((el) => getComputedStyle(el).opacity)).not.toBe('0')
    }).toPass()
  })
}

/** Find a "sort ascending" icon. */
function locateSortAscendingIcon(page: Locator) {
  return page.getByAltText('Sort Ascending')
}

/** Find a "sort descending" icon. */
function locateSortDescendingIcon(page: Locator) {
  return page.getByAltText('Sort Descending')
}

const START_DATE_EPOCH_MS = 1.7e12
/** The number of milliseconds in a minute. */
const MIN_MS = 60_000

test('sort', ({ page }) =>
  mockAllAndLogin({
    page,
    setupAPI: (api) => {
      const date1 = toRfc3339(new Date(START_DATE_EPOCH_MS))
      const date2 = toRfc3339(new Date(START_DATE_EPOCH_MS + 1 * MIN_MS))
      const date3 = toRfc3339(new Date(START_DATE_EPOCH_MS + 2 * MIN_MS))
      const date4 = toRfc3339(new Date(START_DATE_EPOCH_MS + 3 * MIN_MS))
      const date4a = toRfc3339(new Date(START_DATE_EPOCH_MS + 3 * MIN_MS + 1))
      const date4b = toRfc3339(new Date(START_DATE_EPOCH_MS + 3 * MIN_MS + 2))
      const date5 = toRfc3339(new Date(START_DATE_EPOCH_MS + 4 * MIN_MS))
      const date5a = toRfc3339(new Date(START_DATE_EPOCH_MS + 4 * MIN_MS + 1))
      const date6 = toRfc3339(new Date(START_DATE_EPOCH_MS + 5 * MIN_MS))
      const date7 = toRfc3339(new Date(START_DATE_EPOCH_MS + 6 * MIN_MS))
      const date8 = toRfc3339(new Date(START_DATE_EPOCH_MS + 7 * MIN_MS))
      api.addDirectory({ modifiedAt: date4, title: 'a directory 1' })
      api.addDirectory({ modifiedAt: date4a, title: 'a directory 10' })
      api.addDirectory({ modifiedAt: date4b, title: 'a directory 2' })
      api.addDirectory({ modifiedAt: date5a, title: 'a directory 11' })
      api.addDirectory({ modifiedAt: date6, title: 'G directory' })
      api.addProject({ modifiedAt: date7, title: 'C project' })
      api.addSecret({ modifiedAt: date2, title: 'H secret' })
      api.addProject({ modifiedAt: date1, title: 'b project' })
      api.addFile({ modifiedAt: date8, title: 'd file' })
      api.addSecret({ modifiedAt: date3, title: 'f secret' })
      api.addFile({ modifiedAt: date5, title: 'e file' })
      // By date:
      // b project
      // h secret
      // f secret
      // a directory 1
      // a directory 10
      // a directory 2
      // e file
      // a directory 11
      // g directory
      // c project
      // d file
    },
  })
    .driveTable.withNameColumnHeading(async (nameHeading) => {
      await expectOpacity0(locateSortAscendingIcon(nameHeading))
      await expect(locateSortDescendingIcon(nameHeading)).not.toBeVisible()
    })
    .driveTable.withModifiedColumnHeading(async (modifiedHeading) => {
      await expectOpacity0(locateSortAscendingIcon(modifiedHeading))
      await expect(locateSortDescendingIcon(modifiedHeading)).not.toBeVisible()
    })
    .driveTable.withRows(async (rows) => {
      // By default, assets should be grouped by type.
      // Assets in each group are ordered by insertion order.
      await expect(rows).toHaveText([
        /^G directory/,
        /^a directory 11/,
        /^a directory 2/,
        /^a directory 10/,
        /^a directory 1/,
        /^C project/,
        /^b project/,
        /^d file/,
        /^e file/,
        /^f secret/,
        /^H secret/,
      ])
    })
    // Sort by name ascending.
    .driveTable.clickNameColumnHeading()
    .driveTable.withNameColumnHeading(async (nameHeading) => {
      await expectNotOpacity0(locateSortAscendingIcon(nameHeading))
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^a directory 1/,
        /^a directory 2/,
        /^a directory 10/,
        /^a directory 11/,
        /^b project/,
        /^C project/,
        /^d file/,
        /^e file/,
        /^f secret/,
        /^G directory/,
        /^H secret/,
      ])
    })
    // Sort by name descending.
    .driveTable.clickNameColumnHeading()
    .driveTable.withNameColumnHeading(async (nameHeading) => {
      await expectNotOpacity0(locateSortDescendingIcon(nameHeading))
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^H secret/,
        /^G directory/,
        /^f secret/,
        /^e file/,
        /^d file/,
        /^C project/,
        /^b project/,
        /^a directory 11/,
        /^a directory 10/,
        /^a directory 2/,
        /^a directory 1/,
      ])
    })
    // Sorting should be unset.
    .driveTable.clickNameColumnHeading()
    .do(async (thePage) => {
      await thePage.mouse.move(0, 0)
    })
    .driveTable.withNameColumnHeading(async (nameHeading) => {
      await expectOpacity0(locateSortAscendingIcon(nameHeading))
      await expect(locateSortDescendingIcon(nameHeading)).not.toBeVisible()
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^G directory/,
        /^a directory 11/,
        /^a directory 2/,
        /^a directory 10/,
        /^a directory 1/,
        /^C project/,
        /^b project/,
        /^d file/,
        /^e file/,
        /^f secret/,
        /^H secret/,
      ])
    })
    // Sort by date ascending.
    .driveTable.clickModifiedColumnHeading()
    .driveTable.withModifiedColumnHeading(async (modifiedHeading) => {
      await expectNotOpacity0(locateSortAscendingIcon(modifiedHeading))
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^b project/,
        /^H secret/,
        /^f secret/,
        /^a directory 1/,
        /^a directory 10/,
        /^a directory 2/,
        /^e file/,
        /^a directory 11/,
        /^G directory/,
        /^C project/,
        /^d file/,
      ])
    })
    // Sort by date descending.
    .driveTable.clickModifiedColumnHeading()
    .driveTable.withModifiedColumnHeading(async (modifiedHeading) => {
      await expectNotOpacity0(locateSortDescendingIcon(modifiedHeading))
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^d file/,
        /^C project/,
        /^G directory/,
        /^a directory 11/,
        /^e file/,
        /^a directory 2/,
        /^a directory 10/,
        /^a directory 1/,
        /^f secret/,
        /^H secret/,
        /^b project/,
      ])
    })
    // Sorting should be unset.
    .driveTable.clickModifiedColumnHeading()
    .do(async (thePage) => {
      await thePage.mouse.move(0, 0)
    })
    .driveTable.withModifiedColumnHeading(async (modifiedHeading) => {
      await expectOpacity0(locateSortAscendingIcon(modifiedHeading))
      await expect(locateSortDescendingIcon(modifiedHeading)).not.toBeVisible()
    })
    .driveTable.withRows(async (rows) => {
      await expect(rows).toHaveText([
        /^G directory/,
        /^a directory 11/,
        /^a directory 2/,
        /^a directory 10/,
        /^a directory 1/,
        /^C project/,
        /^b project/,
        /^d file/,
        /^e file/,
        /^f secret/,
        /^H secret/,
      ])
    }))
