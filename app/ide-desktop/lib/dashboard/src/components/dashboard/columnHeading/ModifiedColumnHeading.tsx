/** @file A heading for the "Modified" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import TimeIcon from 'enso-assets/time.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

import * as sorting from '#/utilities/sorting'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortInfo, setSortInfo, hideColumn } = state
  const isSortActive = sortInfo?.field === columnUtils.Column.modified
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending

  return (
    <button
      title={
        !isSortActive
          ? 'Sort by modification date'
          : isDescending
            ? 'Stop sorting by modification date'
            : 'Sort by modification date descending'
      }
      className="group flex h-drive-table-heading w-full cursor-pointer items-center gap-icon-with-text"
      onClick={event => {
        event.stopPropagation()
        const nextDirection = isSortActive
          ? sorting.nextSortDirection(sortInfo.direction)
          : sorting.SortDirection.ascending
        if (nextDirection == null) {
          setSortInfo(null)
        } else {
          setSortInfo({ field: columnUtils.Column.modified, direction: nextDirection })
        }
      }}
    >
      <SvgMask
        src={TimeIcon}
        className="size-icon"
        title="Hide this column"
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.modified)
        }}
      />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.modified]}</span>
      <img
        alt={isDescending ? 'Sort Descending' : 'Sort Ascending'}
        src={SortAscendingIcon}
        className={`transition-all duration-arrow ${
          isSortActive ? 'selectable active' : 'transparent group-hover:selectable'
        } ${isDescending ? 'rotate-180' : ''}`}
      />
    </button>
  )
}
