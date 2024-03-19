/** @file A heading for the "Name" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import * as sorting from '#/utilities/sorting'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortInfo, setSortInfo } = state
  const isSortActive = sortInfo?.field === columnUtils.Column.name
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending

  return (
    <button
      title={
        !isSortActive
          ? 'Sort by name'
          : isDescending
            ? 'Stop sorting by name'
            : 'Sort by name descending'
      }
      className="group flex h-drive-table-heading w-full items-center gap-icon-with-text px-name-column-x"
      onClick={event => {
        event.stopPropagation()
        const nextDirection = isSortActive
          ? sorting.nextSortDirection(sortInfo.direction)
          : sorting.SortDirection.ascending
        if (nextDirection == null) {
          setSortInfo(null)
        } else {
          setSortInfo({ field: columnUtils.Column.name, direction: nextDirection })
        }
      }}
    >
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.name]}</span>
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
