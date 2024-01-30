/** @file A heading for the "Name" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'

import * as sorting from '#/utilities/sorting'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortColumn, setSortColumn, sortDirection, setSortDirection } = state
  const [isHovered, setIsHovered] = React.useState(false)
  const isSortActive = sortColumn === columnUtils.Column.name && sortDirection != null
  return (
    <div
      className="flex items-center cursor-pointer gap-2 pt-1 pb-1.5"
      onMouseEnter={() => {
        setIsHovered(true)
      }}
      onMouseLeave={() => {
        setIsHovered(false)
      }}
      onClick={event => {
        event.stopPropagation()
        if (sortColumn === columnUtils.Column.name) {
          setSortDirection(sorting.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
        } else {
          setSortColumn(columnUtils.Column.name)
          setSortDirection(sorting.SortDirection.ascending)
        }
      }}
    >
      <span className="leading-144.5 h-6 py-0.5">
        {columnUtils.COLUMN_NAME[columnUtils.Column.name]}
      </span>
      <img
        src={isSortActive ? columnUtils.SORT_ICON[sortDirection] : SortAscendingIcon}
        className={isSortActive ? '' : isHovered ? 'opacity-50' : 'opacity-0'}
      />
    </div>
  )
}
