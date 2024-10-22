/** @file A heading for the "Name" column. */
import SortAscendingIcon from '#/assets/sort_ascending.svg'
import { Button, Text } from '#/components/AriaComponents'
import type { AssetColumnHeadingProps } from '#/components/dashboard/column'
import { Column } from '#/components/dashboard/column/columnUtils'
import { useText } from '#/providers/TextProvider'
import { SortDirection, nextSortDirection } from '#/utilities/sorting'
import { twMerge } from '#/utilities/tailwindMerge'

/** A heading for the "Name" column. */
export default function NameColumnHeading(props: AssetColumnHeadingProps) {
  const { state } = props
  const { sortInfo, setSortInfo } = state
  const { getText } = useText()
  const isSortActive = sortInfo?.field === Column.name
  const isDescending = sortInfo?.direction === SortDirection.descending

  return (
    <Button
      size="custom"
      variant="custom"
      aria-label={
        !isSortActive ? getText('sortByName')
        : isDescending ?
          getText('stopSortingByName')
        : getText('sortByNameDescending')
      }
      className="group flex h-table-row w-full items-center justify-start gap-icon-with-text px-name-column-x"
      onPress={() => {
        const nextDirection =
          isSortActive ? nextSortDirection(sortInfo.direction) : SortDirection.ascending
        if (nextDirection == null) {
          setSortInfo(null)
        } else {
          setSortInfo({ field: Column.name, direction: nextDirection })
        }
      }}
    >
      <Text className="text-sm font-semibold">{getText('nameColumnName')}</Text>
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
  )
}
