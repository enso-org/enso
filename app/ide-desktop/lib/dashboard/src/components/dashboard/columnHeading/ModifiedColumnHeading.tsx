/** @file A heading for the "Modified" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import TimeIcon from 'enso-assets/time.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

import SortDirection, * as sortDirectionModule from '#/utilities/SortDirection'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortColumn, setSortColumn, sortDirection, setSortDirection } = state
  const [isHovered, setIsHovered] = React.useState(false)
  const isSortActive = sortColumn === columnUtils.Column.modified && sortDirection != null
  return (
    <button
      title={
        !isSortActive
          ? 'Sort by modification date'
          : sortDirection === SortDirection.ascending
          ? 'Sort by modification date descending'
          : 'Stop sorting by modification date'
      }
      className="flex items-center cursor-pointer gap-2"
      onMouseEnter={() => {
        setIsHovered(true)
      }}
      onMouseLeave={() => {
        setIsHovered(false)
      }}
      onClick={event => {
        event.stopPropagation()
        if (sortColumn === columnUtils.Column.modified) {
          setSortDirection(sortDirectionModule.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
        } else {
          setSortColumn(columnUtils.Column.modified)
          setSortDirection(SortDirection.ascending)
        }
      }}
    >
      <SvgMask src={TimeIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">
        {columnUtils.COLUMN_NAME[columnUtils.Column.modified]}
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
