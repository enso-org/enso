/** @file Utilities related to sorting. */

/** Sort direction. */
export enum SortDirection {
    ascending = 'ascending',
    descending = 'descending',
}

/** The next {@link SortDirection}, in the order they are cycled through when clicking a column
 * header. */
export const NEXT_SORT_DIRECTION: Record<SortDirection | 'null', SortDirection | null> = {
    null: SortDirection.ascending,
    [SortDirection.ascending]: SortDirection.descending,
    [SortDirection.descending]: null,
}
