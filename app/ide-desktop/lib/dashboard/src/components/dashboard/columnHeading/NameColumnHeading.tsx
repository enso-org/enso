/** @file A heading for the "Name" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'

import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import SortDirection, * as sortDirectionModule from '#/utilities/SortDirection'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortColumn, setSortColumn, sortDirection, setSortDirection } = state
  const { getText } = textProvider.useText()
  const [isHovered, setIsHovered] = React.useState(false)
  const isSortActive = sortColumn === columnUtils.Column.name && sortDirection != null
  return (
    <button
      title={
        !isSortActive
          ? getText('sortByName')
          : sortDirection === SortDirection.ascending
            ? getText('sortByNameDescending')
            : getText('stopSortingByName')
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
      <span className="leading-144.5 h-6 py-0.5">{getText('nameColumnName')}</span>
      <img
        alt={
          !isSortActive || sortDirection === SortDirection.ascending
            ? getText('sortAscending')
            : getText('sortDescending')
        }
        src={isSortActive ? columnUtils.SORT_ICON[sortDirection] : SortAscendingIcon}
        className={isSortActive ? '' : isHovered ? 'opacity-50' : 'invisible'}
      />
    </button>
  )
}
