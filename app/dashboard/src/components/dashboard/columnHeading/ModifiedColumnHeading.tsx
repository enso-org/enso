/** @file A heading for the "Modified" column. */
import SortAscendingIcon from '#/assets/sort_ascending.svg'
import TimeIcon from '#/assets/time.svg'
import { Text } from '#/components/aria'
import { Button } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useText } from '#/providers/TextProvider'
import { SortDirection, nextSortDirection } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'

/** A heading for the "Modified" column. */
export default function ModifiedColumnHeading(props: AssetColumnHeadingProps) {
  const { state } = props
  const { sortInfo, setSortInfo, hideColumn } = state
  const { getText } = useText()
  const isSortActive = sortInfo?.field === Column.modified
  const isDescending = sortInfo?.direction === SortDirection.descending

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
        aria-label={getText('modifiedColumnHide')}
        tooltip={false}
        onPress={() => {
          hideColumn(Column.modified)
        }}
      />
      <Button
        size="custom"
        variant="custom"
        className="flex grow justify-start gap-icon-with-text"
        onPress={() => {
          const nextDirection =
            isSortActive ? nextSortDirection(sortInfo.direction) : SortDirection.ascending
          if (nextDirection == null) {
            setSortInfo(null)
          } else {
            setSortInfo({ field: Column.modified, direction: nextDirection })
          }
        }}
      >
        <Text className="text-header">{getText('modifiedColumnName')}</Text>
        <img
          alt={isDescending ? getText('sortDescending') : getText('sortAscending')}
          src={SortAscendingIcon}
          className={twMerge(
            'transition-all duration-arrow',
            isSortActive ? 'selectable active' : 'opacity-0 group-hover:selectable',
            isDescending && 'rotate-180',
          )}
        />
      </Button>
    </div>
  )
}
