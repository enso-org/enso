/** @file A heading for the "Modified" column. */
import { Button, Text } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { SortDirection, iconIdFor, nextSortDirection } from '#/utilities/sorting'
import { twJoin } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn, sortInfo, setSortInfo } = props

  const { getText } = useText()

  const isSortActive = sortInfo?.field === Column.modified
  const isDescending = sortInfo?.direction === SortDirection.descending

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.modified)
  })

  const cycleSortDirection = useEventCallback(() => {
    if (!sortInfo) {
      setSortInfo({ field: Column.modified, direction: SortDirection.ascending })
      return
    }

    const nextDirection =
      isSortActive ? nextSortDirection(sortInfo.direction) : SortDirection.ascending

    if (nextDirection == null) {
      setSortInfo(null)
    } else {
      setSortInfo({ field: Column.modified, direction: nextDirection })
    }
  })

  return (
    <div
      aria-label={
        !isSortActive ? getText('sortByModificationDate')
        : isDescending ?
          getText('stopSortingByModificationDate')
        : getText('sortByModificationDateDescending')
      }
      className="group flex h-table-row w-full cursor-pointer items-center gap-2"
    >
      <Button
        variant="icon"
        icon="time"
        aria-label={getText('hideColumn')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Button
        fullWidth
        size="custom"
        variant="custom"
        addonEnd={
          <Icon
            icon={iconIdFor(sortInfo?.direction, isSortActive)}
            className={twJoin(
              'ml-1 transition-all duration-arrow',
              isSortActive ? 'selectable active' : 'opacity-0 group-hover:selectable',
            )}
          />
        }
        className="flex justify-start"
        onPress={cycleSortDirection}
      >
        <Text weight="bold">{getText('modifiedColumnName')}</Text>
      </Button>
    </div>
  )
}
