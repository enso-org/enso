/** @file Utilities related to sorting. */

// =====================
// === SortDirection ===
// =====================

/** Sort direction. */
enum SortDirection {
  ascending = 'ascending',
  descending = 'descending',
}

// This is REQUIRED, as `export default enum` is invalid syntax.
// eslint-disable-next-line no-restricted-syntax
export default SortDirection

/** The next {@link SortDirection}, in the order they are cycled through when clicking a column
 * header. */
export const NEXT_SORT_DIRECTION: Record<SortDirection | 'null', SortDirection | null> = {
  null: SortDirection.ascending,
  [SortDirection.ascending]: SortDirection.descending,
  [SortDirection.descending]: null,
}
