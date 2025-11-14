/** @file Utilities related to sorting. */
import type { SvgUseIcon } from '#/components/types'
import type { AssetSortDirection } from 'enso-common/src/services/Backend'

/** Sort direction for assets. */
export type SortDirection = AssetSortDirection

/**
 * The next {@link AssetSortDirection}, in the order they are cycled through when clicking a column
 * header.
 */
export function nextSortDirection(
  sortDirection: AssetSortDirection | null,
): AssetSortDirection | null {
  switch (sortDirection) {
    case null: {
      return 'ascending'
    }
    case 'ascending': {
      return 'descending'
    }
    case 'descending': {
      return null
    }
  }
}

/** The corresponding icon id forr a given {@link SortDirection}. */
export function iconIdFor(
  sortDirection: AssetSortDirection | null | undefined,
  sortInfoAppliesToCurrentColumn = true,
): SvgUseIcon {
  if (!sortInfoAppliesToCurrentColumn) {
    return 'sort'
  }
  switch (sortDirection) {
    case null:
    case undefined: {
      return 'sort'
    }
    case 'ascending': {
      return 'sort_ascending'
    }
    case 'descending': {
      return 'sort_descending'
    }
  }
}

/** Sort information. */
export interface SortInfo<Field extends string> {
  readonly field: Field
  readonly direction: AssetSortDirection
}
