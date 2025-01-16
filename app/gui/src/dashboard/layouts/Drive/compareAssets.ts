/** @file Functions related to comparing assets. */
import { Column, type SortableColumn } from '#/components/dashboard/column/columnUtils'
import { SortDirection, type SortInfo } from '#/utilities/sorting'
import type { AnyAsset } from 'enso-common/src/services/Backend'

/** Return a function to compare two assets. */
export function assetCompareFunction(
  sortInfo: SortInfo<SortableColumn>,
  locale: string | undefined,
) {
  const multiplier = sortInfo.direction === SortDirection.ascending ? 1 : -1
  let compare: (a: AnyAsset, b: AnyAsset) => number
  switch (sortInfo.field) {
    case Column.name: {
      compare = (a, b) => multiplier * a.title.localeCompare(b.title, locale, { numeric: true })
      break
    }
    case Column.modified: {
      compare = (a, b) => {
        const aOrder = Number(new Date(a.modifiedAt))
        const bOrder = Number(new Date(b.modifiedAt))
        return multiplier * (aOrder - bOrder)
      }
      break
    }
  }
  return compare
}
