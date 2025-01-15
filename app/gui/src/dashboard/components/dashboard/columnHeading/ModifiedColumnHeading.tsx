/** @file A heading for the "Modified" column. */
import SortAscendingIcon from '#/assets/sort_ascending.svg'
import TimeIcon from '#/assets/time.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useText } from '#/providers/TextProvider'
import { SortDirection, nextSortDirection } from '#/utilities/sorting'
import { twJoin } from '#/utilities/tailwindMerge'

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
      className="group flex h-table-row w-full cursor-pointer items-center gap-icon-with-text"
    >
      <Button
        variant="icon"
        icon={TimeIcon}
        aria-label={getText('hideColumn')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Button
        size="custom"
        variant="custom"
        className="flex grow justify-start gap-icon-with-text"
        onPress={cycleSortDirection}
      >
        <Text weight="bold" truncate="1" color="custom">
          {getText('modifiedColumnName')}
        </Text>

        <img
          alt={isDescending ? getText('sortDescending') : getText('sortAscending')}
          src={SortAscendingIcon}
          className={twJoin(
            'transition-all duration-arrow',
            isSortActive ? 'selectable active' : 'opacity-0 group-hover:selectable',
            isDescending && 'rotate-180',
          )}
        />
      </Button>
    </div>
  )
}
