/** @file Components for column headings. */
import { Button } from '#/components/Button'
import { Icon } from '#/components/Icon'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { AssetColumnHeadingProps } from '#/pages/dashboard/components/column'
import { Column } from '#/pages/dashboard/components/column/columnUtils'
import { iconIdFor, nextSortDirection } from '#/utilities/sorting'
import { twJoin } from '#/utilities/tailwindMerge'
import { useText } from '$/providers/react'

/** A heading for the "Accessed by projects" column. */
export function AccessedByProjectsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.accessedByProjects)
  })

  return (
    <div className="flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="accessed_by_projects"
        aria-label={getText('accessedByProjectsColumnName')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('accessedByProjectsColumnName')}
      </Text>
    </div>
  )
}

/** A heading for the "Accessed data" column. */
export function AccessedDataColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props
  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.accessedData)
  })

  return (
    <div className="flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="accessed_data"
        aria-label={getText('accessedDataColumnName')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('accessedDataColumnName')}
      </Text>
    </div>
  )
}

/** A heading for the "Labels" column. */
export function LabelsColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.labels)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="tag"
        aria-label={getText('labelsColumnName')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('labelsColumnName')}
      </Text>
    </div>
  )
}

/** A heading for the "Modified" column. */
export function ModifiedColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn, sortInfo, setSortInfo } = props

  const { getText } = useText()

  const isSortActive = sortInfo?.field === 'modified_at'
  const isDescending = sortInfo?.direction === 'descending'

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.modified)
  })

  const cycleSortDirection = useEventCallback(() => {
    if (!sortInfo) {
      setSortInfo({ field: 'modified_at', direction: 'ascending' })
      return
    }
    const nextDirection = isSortActive ? nextSortDirection(sortInfo.direction) : 'ascending'
    if (nextDirection == null) {
      setSortInfo(null)
    } else {
      setSortInfo({ field: 'modified_at', direction: nextDirection })
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

/** A heading for the "Name" column. */
export function NameColumnHeading(props: AssetColumnHeadingProps) {
  const { sortInfo, setSortInfo } = props

  const { getText } = useText()
  const isSortActive = sortInfo?.field === 'title'
  const isDescending = sortInfo?.direction === 'descending'

  const cycleSortDirection = useEventCallback(() => {
    if (!sortInfo) {
      setSortInfo({ field: 'title', direction: 'ascending' })
      return
    }
    const nextDirection = isSortActive ? nextSortDirection(sortInfo.direction) : 'ascending'
    if (nextDirection == null) {
      setSortInfo(null)
    } else {
      setSortInfo({ field: 'title', direction: nextDirection })
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

/** A heading for the "Path" column. */
export function PathColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.path)
  })

  return (
    <div
      className="isolate flex h-table-row w-full items-center gap-2"
      data-testid="path-column-heading"
    >
      <Button
        variant="icon"
        icon="folder"
        aria-label={getText('pathColumnName')}
        tooltip={false}
        onPress={hideThisColumn}
      />
      <Text weight="bold" truncate="1" color="custom">
        {getText('pathColumnName')}
      </Text>
    </div>
  )
}

/** A heading for the "Shared with" column. */
export function SharedWithColumnHeading(props: AssetColumnHeadingProps) {
  const { hideColumn } = props

  const { getText } = useText()

  const hideThisColumn = useEventCallback(() => {
    hideColumn(Column.sharedWith)
  })

  return (
    <div className="isolate flex h-table-row w-full items-center gap-2">
      <Button
        variant="icon"
        icon="people"
        aria-label={getText('sharedWithColumnName')}
        tooltip={false}
        onPress={hideThisColumn}
      />

      <div className="flex items-center gap-1">
        <Text weight="bold" truncate="1" color="custom">
          {getText('sharedWithColumnName')}
        </Text>
      </div>
    </div>
  )
}
