/** @file Utilities related to sorting. */

// =====================
// === SortDirection ===
// =====================

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

// ================
// === SortInfo ===
// ================

/** Sort information. */
export interface SortInfo<Field> {
  readonly field: Field
  readonly direction: SortDirection
}
