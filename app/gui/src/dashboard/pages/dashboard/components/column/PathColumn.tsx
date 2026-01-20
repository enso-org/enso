/** @file A column displaying the path of the asset. */
import FolderArrowIcon from '#/assets/folder_arrow.svg'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { Icon } from '#/components/Icon'
import SvgMask from '#/components/SvgMask'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useCategories, type AnyCloudCategory } from '#/layouts/Drive/Categories'
import type { AssetColumnProps } from '#/pages/dashboard/components/column'
import { setDriveLocation } from '#/providers/DriveProvider'
import { useUser } from '$/providers/react'
import type { DirectoryId } from 'enso-common/src/services/Backend'
import { parseDirectoriesPath } from 'enso-common/src/services/Backend/utilities'
import { Fragment, useTransition } from 'react'
import invariant from 'tiny-invariant'

/** A column displaying the path of the asset. */
export function PathColumn(props: AssetColumnProps) {
  const { item } = props
  const { virtualParentsPath, parentsPath } = item

  const { rootDirectoryId } = useUser()
  const { getCategoryByDirectoryId } = useCategories()

  const { finalPath } = parseDirectoriesPath({
    parentsPath,
    virtualParentsPath,
    rootDirectoryId,
    getCategoryByDirectoryId,
  })

  const navigateToDirectory = useEventCallback((targetDirectory: DirectoryId) => {
    const targetDirectoryIndex = finalPath.findIndex(({ id }) => id === targetDirectory)
    const targetDirectoryInfo = finalPath[targetDirectoryIndex]
    if (targetDirectoryIndex === -1 || !targetDirectoryInfo) {
      return
    }
    const pathToDirectory = finalPath
      .slice(0, targetDirectoryIndex + 1)
      .map(({ id, categoryId }) => ({ id, categoryId }))
    const rootDirectoryInThePath = pathToDirectory[0]
    // This should never happen, as we always have the root directory in the path.
    invariant(rootDirectoryInThePath, 'Root directory id is null')
    setDriveLocation(targetDirectory, rootDirectoryInThePath.categoryId)
  })

  const firstItemInPath = finalPath.at(0)
  const lastItemInPath = finalPath.at(-1)
  // This will be true if `finalPath` is empty.
  if (lastItemInPath == null || firstItemInPath == null) {
    return <></>
  }

  // If the first and last item are the same, then there is only one entry.
  if (firstItemInPath === lastItemInPath) {
    return (
      <div
        className="contents"
        data-testid={`path-column-cell-${item.title.toLowerCase().replace(/\s+/g, '-')}`}
      >
        <PathItem
          id={lastItemInPath.id}
          label={lastItemInPath.label}
          icon={lastItemInPath.icon}
          onNavigate={navigateToDirectory}
        />
      </div>
    )
  }

  return (
    <div data-testid={`path-column-cell-${item.title.toLowerCase().replace(/\s+/g, '-')}`}>
      <Popover.Trigger>
        <Button variant="ghost-fading" size="xsmall">
          <div className="flex items-center gap-2">
            <Icon className="h-3 w-3" icon={firstItemInPath.icon} />
            <Icon className="h-3 w-3" icon={FolderArrowIcon} />
            <Icon className="h-3 w-3" icon={lastItemInPath.icon} />

            <Text color="custom" truncate="1" className="max-w-48">
              {lastItemInPath.label}
            </Text>
          </div>
        </Button>

        <Popover
          size="auto"
          placement="bottom end"
          crossOffset={14}
          className="max-w-lg"
          rounded="xxxlarge"
        >
          <div className="flex items-center gap-1">
            {finalPath.map((entry, index) => (
              <Fragment key={entry.id}>
                <PathItem
                  id={entry.id}
                  label={entry.label}
                  icon={entry.icon}
                  onNavigate={navigateToDirectory}
                />

                {index < finalPath.length - 1 && (
                  <SvgMask src={FolderArrowIcon} className="h-4 w-4 text-primary" />
                )}
              </Fragment>
            ))}
          </div>
        </Popover>
      </Popover.Trigger>
    </div>
  )
}

/** Props for the {@link PathItem} component. */
interface PathItemProps {
  readonly id: DirectoryId
  readonly label: AnyCloudCategory['label']
  readonly icon: AnyCloudCategory['icon']
  readonly onNavigate: (targetDirectory: DirectoryId) => void
}

/** Individual item in the path. */
function PathItem(props: PathItemProps) {
  const { id, label, icon, onNavigate } = props
  const [transition, startTransition] = useTransition()

  const onPress = useEventCallback(() => {
    startTransition(() => {
      onNavigate(id)
    })
  })

  return (
    <Button
      key={id}
      variant="ghost-fading"
      size="small"
      isLoading={transition}
      icon={icon}
      onPress={onPress}
      loaderPosition="icon"
      testId={`path-column-item-${label.toLowerCase().replace(/\s+/g, '-')}`}
    >
      <Text color="custom" truncate="1" className="max-w-48">
        {label}
      </Text>
    </Button>
  )
}
