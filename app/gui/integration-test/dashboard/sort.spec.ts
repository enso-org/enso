/** @file Test sorting of assets columns. */
import { expect, test } from '@playwright/test'

import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'

import { mockAllAndLogin } from './actions'

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
