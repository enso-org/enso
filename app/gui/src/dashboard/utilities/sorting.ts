/** @file Utilities related to sorting. */
import type { SvgUseIcon } from '#/components/AriaComponents'

/** Sort direction. */
export enum SortDirection {
  ascending = 'ascending',
  descending = 'descending',
}

/**
 * The next {@link SortDirection}, in the order they are cycled through when clicking a column
 * header.
 */
export function nextSortDirection(sortDirection: SortDirection | null) {
  switch (sortDirection) {
    case null: {
      return SortDirection.ascending
    }
    case SortDirection.ascending: {
      return SortDirection.descending
    }
    case SortDirection.descending: {
      return null
    }
  }
}

/** The corresponding icon id forr a given {@link SortDirection}. */
export function iconIdFor(
  sortDirection: SortDirection | null | undefined,
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
    case SortDirection.ascending: {
      return 'sort_ascending'
    }
    case SortDirection.descending: {
      return 'sort_descending'
    }
  }
}

/** Sort information. */
export interface SortInfo<Field> {
  readonly field: Field
  readonly direction: SortDirection
}
