/** @file A heading for the "Modified" column. */
import * as React from 'react'

import SortAscendingIcon from '#/assets/sort_ascending.svg'
import TimeIcon from '#/assets/time.svg'
import * as tailwindMerge from 'tailwind-merge'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

import * as sorting from '#/utilities/sorting'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(
  props: column.AssetColumnHeadingProps
): React.JSX.Element {
  const { state } = props
  const { sortInfo, setSortInfo, hideColumn } = state
  const { getText } = textProvider.useText()
  const isSortActive = sortInfo?.field === columnUtils.Column.modified
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending

  return (
    <div
      aria-label={
        !isSortActive
          ? getText('sortByModificationDate')
          : isDescending
            ? getText('stopSortingByModificationDate')
            : getText('sortByModificationDateDescending')
      }
      className="group flex h-drive-table-heading w-full cursor-pointer items-center gap-icon-with-text"
    >
      <Button
        active
        image={TimeIcon}
        className="size-icon"
        alt={getText('modifiedColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.modified)
        }}
      />
      <ariaComponents.Button
        size="custom"
        variant="custom"
        className="flex grow justify-start gap-icon-with-text"
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
        <aria.Text className="text-header">{getText('modifiedColumnName')}</aria.Text>
        <img
          alt={isDescending ? getText('sortDescending') : getText('sortAscending')}
          src={SortAscendingIcon}
          className={tailwindMerge.twMerge(
            'transition-all duration-arrow',
            isSortActive ? 'selectable active' : 'opacity-0 group-hover:selectable',
            isDescending && 'rotate-180'
          )}
        />
      </ariaComponents.Button>
    </div>
  )
}
