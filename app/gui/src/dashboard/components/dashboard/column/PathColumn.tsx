/** @file A column displaying the path of the asset. */
import FolderArrowIcon from '#/assets/folder_arrow.svg'
import { Button, Popover, Text } from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useCategoriesAPI, useCloudCategoryList } from '#/layouts/Drive/Categories/categoriesHooks'
import type { AnyCloudCategory } from '#/layouts/Drive/Categories/Category'
import { useUser } from '#/providers/AuthProvider'
import { useSetCurrentDirectoryId } from '#/providers/DriveProvider'
import type { DirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { Fragment, useTransition } from 'react'
import invariant from 'tiny-invariant'
import type { AssetColumnProps } from '../column'

/** A column displaying the path of the asset. */
export default function PathColumn(props: AssetColumnProps) {
  const { item, state } = props

  const { virtualParentsPath, parentsPath } = item

  const { getAssetNodeById } = state

  const { setCategory } = useCategoriesAPI()
  const setCurrentDirectoryId = useSetCurrentDirectoryId()
  const { rootDirectoryId } = useUser()

  // Path navigation exist only for cloud categories.
  const { getCategoryByDirectoryId } = useCloudCategoryList()

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
    // If it happens, it means you've skrewed up
    invariant(rootDirectoryInThePath != null, 'Root directory id is null')

    // If the target directory is null, we assume that this directory is outside of the current tree (in another category).
    // Which is the default, because the path is only displayed in the Recent and Trash folders.
    // But sometimes the user might delete a directory with its whole content,
    // and in that case it will be present in the tree,
    // because the parent is always fetched before its children.
    const targetDirectoryNode = getAssetNodeById(targetDirectory)

    if (targetDirectoryNode == null && rootDirectoryInThePath.categoryId != null) {
      setCategory(rootDirectoryInThePath.categoryId)
    }

    setCurrentDirectoryId({
      current: targetDirectory,
      parent: finalPath[targetDirectoryIndex - 1]?.id ?? null,
    })
  })

  if (finalPath.length === 0) {
    return <></>
  }

  const firstItemInPath = finalPath.at(0)
  const lastItemInPath = finalPath.at(-1)

  // Should not happen, as we ensure that the final path is not empty.
  if (lastItemInPath == null || firstItemInPath == null) {
    return <></>
  }

  // This also means that the first and the last item in the path are the same
  if (finalPath.length === 1) {
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
            <SvgMask src={firstItemInPath.icon} className="h-3 w-3" />
            <SvgMask src={FolderArrowIcon} className="h-3 w-3" />
            <SvgMask src={lastItemInPath.icon} className="h-3 w-3" />

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
      loading={transition}
      icon={icon}
      onPress={onPress}
      loaderPosition="icon"
      data-testid={`path-column-item-${label.toLowerCase().replace(/\s+/g, '-')}`}
    >
      <Text color="custom" truncate="1" className="max-w-48">
        {label}
      </Text>
    </Button>
  )
}
