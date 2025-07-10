/** @file Functions related to comparing assets. */
import type { SortInfo } from '#/utilities/sorting'
import type { AnyAsset, AssetSortExpression } from 'enso-common/src/services/Backend'

/** Return a function to compare two assets. */
export function assetCompareFunction(
  sortInfo: SortInfo<AssetSortExpression>,
  locale: string | undefined,
) {
  const multiplier = sortInfo.direction === 'ascending' ? 1 : -1
  let compare: (a: AnyAsset, b: AnyAsset) => number
  switch (sortInfo.field) {
    case 'title': {
      compare = (a, b) => multiplier * a.title.localeCompare(b.title, locale, { numeric: true })
      break
    }
    case 'modified_at': {
      compare = (a, b) => {
        const aOrder = Number(new Date(a.modifiedAt))
        const bOrder = Number(new Date(b.modifiedAt))
        return multiplier * (aOrder - bOrder)
      }
      break
    }
    case 'asset_discriminator_and_id': {
      compare = (a, b) =>
        multiplier *
        (a.id > b.id ? 1
        : a.id < b.id ? -1
        : 0)
      break
    }
  }
  return compare
}
