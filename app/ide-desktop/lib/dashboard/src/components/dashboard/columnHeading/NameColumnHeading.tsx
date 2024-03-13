/** @file A heading for the "Name" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import SortDirection, * as sortDirectionModule from '#/utilities/SortDirection'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortColumn, setSortColumn, sortDirection, setSortDirection } = state
  const isSortActive = sortColumn === columnUtils.Column.name && sortDirection != null
  const isDescending = sortDirection === SortDirection.descending

  return (
    <button
      title={
        !isSortActive
          ? 'Sort by name'
          : sortDirection === SortDirection.ascending
            ? 'Sort by name descending'
            : 'Stop sorting by name'
      }
      className="group flex h-drive-table-heading w-full items-center gap-icon-with-text px-name-column-x"
      onClick={event => {
        event.stopPropagation()
        if (sortColumn === columnUtils.Column.name) {
          setSortDirection(sortDirectionModule.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
        } else {
          setSortColumn(columnUtils.Column.name)
          setSortDirection(SortDirection.ascending)
        }
      }}
    >
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.name]}</span>
      <img
        alt={
          !isSortActive || sortDirection === SortDirection.ascending
            ? 'Sort Ascending'
            : 'Sort Descending'
        }
        src={SortAscendingIcon}
        className={`transition-all duration-arrow ${
          isSortActive ? 'selectable active' : 'transparent group-hover:selectable'
        } ${isDescending ? 'rotate-180' : ''}`}
      />
    </button>
  )
}
