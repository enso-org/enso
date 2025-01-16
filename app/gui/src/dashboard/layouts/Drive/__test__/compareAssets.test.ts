/** @file Tests for comparing assets. */
import { Column, type SortableColumn } from '#/components/dashboard/column/columnUtils'
import { assetCompareFunction } from '#/layouts/Drive/compareAssets'
import { SortDirection, type SortInfo } from '#/utilities/sorting'
import * as fc from '@fast-check/vitest'
import { DirectoryId, createPlaceholderFileAsset } from 'enso-common/src/services/Backend'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { merge } from 'enso-common/src/utilities/data/object'
import { expect } from 'vitest'

const SORT_BY_NAME_ASCENDING: SortInfo<SortableColumn> = {
  field: Column.name,
  direction: SortDirection.ascending,
}

const SORT_BY_NAME_DESCENDING: SortInfo<SortableColumn> = {
  field: Column.name,
  direction: SortDirection.descending,
}

const SORT_BY_MODIFIED_ASCENDING: SortInfo<SortableColumn> = {
  field: Column.modified,
  direction: SortDirection.ascending,
}

const SORT_BY_MODIFIED_DESCENDING: SortInfo<SortableColumn> = {
  field: Column.modified,
  direction: SortDirection.descending,
}

fc.test.prop({
  prefix: fc.fc.string(),
  numbers: fc.fc.array(fc.fc.integer({ min: 0 })),
})('numbers should be sorted with dictionary sort', ({ prefix, numbers }) => {
  const names = numbers.map((number) => `${prefix} ${number}`)
  const assets = names.map((name) =>
    createPlaceholderFileAsset(name, DirectoryId('directory-'), []),
  )
  const compareByNameAscending = assetCompareFunction(SORT_BY_NAME_ASCENDING, 'en')
  const sorted = assets.sort(compareByNameAscending)
  const sortedNames = sorted.map((asset) => asset.title)
  const expectedNames = numbers.sort((a, b) => a - b).map((number) => `${prefix} ${number}`)
  expect(sortedNames).toStrictEqual(expectedNames)

  const compareByNameDescending = assetCompareFunction(SORT_BY_NAME_DESCENDING, 'en')
  const sortedDescending = assets.sort(compareByNameDescending)
  const sortedDescendingNames = sortedDescending.map((asset) => asset.title)
  const expectedDescendingNames = numbers
    .sort((a, b) => b - a)
    .map((number) => `${prefix} ${number}`)
  expect(sortedDescendingNames).toStrictEqual(expectedDescendingNames)
})

fc.test.prop({
  names: fc.fc.array(fc.fc.string().map((s) => s.replace(/\d+/g, ''))),
})('sort by name', ({ names }) => {
  const assets = names.map((name) =>
    createPlaceholderFileAsset(name, DirectoryId('directory-'), []),
  )

  const compareByModifiedAscending = assetCompareFunction(SORT_BY_NAME_ASCENDING, 'en')
  const sorted = assets.sort(compareByModifiedAscending)
  const sortedNames = sorted.map((asset) => asset.title)
  const expectedNames = names.sort((a, b) => a.localeCompare(b, 'en'))
  expect(sortedNames).toStrictEqual(expectedNames)

  const compareByModifiedDescending = assetCompareFunction(SORT_BY_NAME_DESCENDING, 'en')
  const sortedDescending = assets.sort(compareByModifiedDescending)
  const sortedDescendingNames = sortedDescending.map((asset) => asset.title)
  const expectedDescendingNames = names.sort((a, b) => -a.localeCompare(b, 'en'))
  expect(sortedDescendingNames).toStrictEqual(expectedDescendingNames)
})

fc.test.prop({
  dates: fc.fc.array(fc.fc.integer({ min: 0 })).map((numbers) => numbers.map((n) => new Date(n))),
})('sort by modified', ({ dates }) => {
  const assets = dates.map((date) =>
    merge(createPlaceholderFileAsset('', DirectoryId('directory-'), []), {
      modifiedAt: toRfc3339(date),
    }),
  )

  const compareByModifiedAscending = assetCompareFunction(SORT_BY_MODIFIED_ASCENDING, 'en')
  const sorted = assets.sort(compareByModifiedAscending)
  const sortedDates = sorted.map((asset) => new Date(asset.modifiedAt))
  const expectedDates = dates.sort((a, b) => Number(a) - Number(b))
  expect(sortedDates).toStrictEqual(expectedDates)

  const compareByModifiedDescending = assetCompareFunction(SORT_BY_MODIFIED_DESCENDING, 'en')
  const sortedDescending = assets.sort(compareByModifiedDescending)
  const sortedDescendingDates = sortedDescending.map((asset) => new Date(asset.modifiedAt))
  const expectedDescendingDates = dates.sort((a, b) => Number(b) - Number(a))
  expect(sortedDescendingDates).toStrictEqual(expectedDescendingDates)
})
