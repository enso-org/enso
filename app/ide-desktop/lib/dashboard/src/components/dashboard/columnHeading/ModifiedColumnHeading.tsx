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
  const { sortColumn, setSortColumn, sortDirection, setSortDirection, hideColumn } = state
  const isSortActive = sortColumn === columnUtils.Column.modified && sortDirection != null
  const isDescending = sortDirection === SortDirection.descending

  return (
    <button
      title={
        !isSortActive
          ? 'Sort by modification date'
          : sortDirection === SortDirection.ascending
            ? 'Sort by modification date descending'
            : 'Stop sorting by modification date'
      }
      className="group flex h-drive-table-heading w-full cursor-pointer items-center gap-icon-with-text"
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
      <SvgMask
        src={TimeIcon}
        className="size-icon"
        alt="Hide Modified date"
        title="Hide Modified date"
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.modified)
        }}
      />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.modified]}</span>
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
