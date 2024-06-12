/** @file A heading for the "Name" column. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import SortAscendingIcon from 'enso-assets/sort_ascending.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'

import * as sorting from '#/utilities/sorting'

/** A heading for the "Name" column. */
export default function NameColumnHeading(
  props: column.AssetColumnHeadingProps
): React.JSX.Element {
  const { state } = props
  const { sortInfo, setSortInfo } = state
  const { getText } = textProvider.useText()
  const isSortActive = sortInfo?.field === columnUtils.Column.name
  const isDescending = sortInfo?.direction === sorting.SortDirection.descending

  return (
    <ariaComponents.Button
      size="custom"
      variant="custom"
      aria-label={
        !isSortActive
          ? getText('sortByName')
          : isDescending
            ? getText('stopSortingByName')
            : getText('sortByNameDescending')
      }
      className="group flex h-table-row w-full items-center justify-start gap-icon-with-text px-name-column-x"
      onPress={() => {
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
      <aria.Text className="text-header">{getText('nameColumnName')}</aria.Text>
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
  )
}
