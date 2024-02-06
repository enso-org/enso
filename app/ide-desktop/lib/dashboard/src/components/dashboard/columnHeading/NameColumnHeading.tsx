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
  const [isHovered, setIsHovered] = React.useState(false)
  const isSortActive = sortColumn === columnUtils.Column.name && sortDirection != null
  return (
    <button
      title={
        !isSortActive
          ? 'Sort by name'
          : sortDirection === SortDirection.ascending
          ? 'Sort by name descending'
          : 'Stop sorting by name'
      }
      className="flex items-center gap-2 pt-1 pb-1.5"
      onMouseEnter={() => {
        setIsHovered(true)
      }}
      onMouseLeave={() => {
        setIsHovered(false)
      }}
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
      <span className="leading-144.5 h-6 py-0.5">
        {columnUtils.COLUMN_NAME[columnUtils.Column.name]}
      </span>
      <img
        alt={
          !isSortActive || sortDirection === SortDirection.ascending
            ? 'Sort Ascending'
            : 'Sort Descending'
        }
        src={isSortActive ? columnUtils.SORT_ICON[sortDirection] : SortAscendingIcon}
        className={isSortActive ? '' : isHovered ? 'opacity-50' : 'invisible'}
      />
    </button>
  )
}
