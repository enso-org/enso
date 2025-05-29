/** @file A heading for the "Name" column. */
import { Button } from '#/components/Button'
import { Icon } from '#/components/Icon'
import { Text } from '#/components/Text'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { SortDirection, iconIdFor, nextSortDirection } from '#/utilities/sorting'
import { twJoin } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: AssetColumnHeadingProps) {
  const { sortInfo, setSortInfo } = props

  const { getText } = useText()
  const isSortActive = sortInfo?.field === Column.name
  const isDescending = sortInfo?.direction === SortDirection.descending

  const cycleSortDirection = useEventCallback(() => {
    if (!sortInfo) {
      setSortInfo({ field: Column.name, direction: SortDirection.ascending })
      return
    }

    const nextDirection =
      isSortActive ? nextSortDirection(sortInfo.direction) : SortDirection.ascending
    if (nextDirection == null) {
      setSortInfo(null)
    } else {
      setSortInfo({ field: Column.name, direction: nextDirection })
    }
  })

  return (
    <Button
      fullWidth
      size="custom"
      variant="custom"
      aria-label={
        !isSortActive ? getText('sortByName')
        : isDescending ?
          getText('stopSortingByName')
        : getText('sortByNameDescending')
      }
      addonEnd={
        <Icon
          icon={iconIdFor(sortInfo?.direction, isSortActive)}
          className={twJoin(
            'ml-1 transition-all duration-arrow',
            isSortActive ? 'selectable active' : 'opacity-0 group-hover:selectable',
          )}
        />
      }
      className="group sticky left-0 flex h-table-row justify-start bg-dashboard px-name-column-x"
      onPress={cycleSortDirection}
    >
      <Text weight="bold">{getText('nameColumnName')}</Text>
    </Button>
  )
}
