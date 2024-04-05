/** @file A heading for the "Modified" column. */
import * as React from 'react'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import TimeIcon from 'enso-assets/time.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'
import UnstyledButton from '#/components/UnstyledButton'

import * as sorting from '#/utilities/sorting'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(props: column.AssetColumnHeadingProps): JSX.Element {
  const { state } = props
  const { sortInfo, setSortInfo, hideColumn } = state
  const { getText } = textProvider.useText()
  const isSortActive = sortInfo?.field === columnUtils.Column.modified
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending

  return (
    <UnstyledButton
      aria-label={
        !isSortActive
          ? getText('sortByModificationDate')
          : isDescending
            ? getText('stopSortingByModificationDate')
            : getText('sortByModificationDateDescending')
      }
      className="group flex h-drive-table-heading w-full cursor-pointer items-center gap-icon-with-text"
      onPress={() => {
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
        alt={getText('modifiedColumnHide')}
        title={getText('modifiedColumnHide')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.modified)
        }}
      />
      <aria.Text className="text-header">{getText('modifiedColumnName')}</aria.Text>
      <img
        alt={isDescending ? getText('sortDescending') : getText('sortAscending')}
        src={SortAscendingIcon}
        className={`transition-all duration-arrow ${
          isSortActive ? 'selectable active' : 'transparent group-hover:selectable'
        } ${isDescending ? 'rotate-180' : ''}`}
      />
    </UnstyledButton>
  )
}
