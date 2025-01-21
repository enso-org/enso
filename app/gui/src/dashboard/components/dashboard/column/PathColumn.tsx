/** @file A column displaying the path of the asset. */
import FolderIcon from '#/assets/folder.svg'
import FolderArrowIcon from '#/assets/folder_arrow.svg'
import { Button, Popover, Text } from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useCategoriesAPI, useCloudCategoryList } from '#/layouts/Drive/Categories/categoriesHooks'
import type { AnyCloudCategory } from '#/layouts/Drive/Categories/Category'
import { useUser } from '#/providers/AuthProvider'
import { useSetExpandedDirectoryIds, useSetSelectedAssets } from '#/providers/DriveProvider'
import { AssetType, DirectoryId, isDirectoryId } from '#/services/Backend'
import { Fragment, useTransition } from 'react'
import invariant from 'tiny-invariant'
import type { AssetColumnProps } from '../column'

/** A column displaying the path of the asset. */
export default function PathColumn(props: AssetColumnProps) {
  const { item, state } = props

  const { virtualParentsPath, parentsPath } = item

  const { getAssetNodeById } = state

  const { setCategory } = useCategoriesAPI()
  const setSelectedAssets = useSetSelectedAssets()
  const setExpandedDirectoryIds = useSetExpandedDirectoryIds()

  // Path navigation exist only for cloud categories.
  const { getCategoryByDirectoryId } = useCloudCategoryList()

  // Parents path is a string of directory ids separated by slashes.
  const splitPath = parentsPath.split('/').filter(isDirectoryId)
  const rootDirectoryInPath = splitPath[0]

  const splitVirtualParentsPath = virtualParentsPath.split('/')
  // Virtual parents path is a string of directory names separated by slashes.
  // To match the ids with the names, we need to remove the first element of the split path.
  // As the first element is the root directory, which is not a virtual parent.
  const virtualParentsIds = splitPath.slice(1)

  const { rootDirectoryId } = useUser()

  const navigateToDirectory = useEventCallback((targetDirectory: DirectoryId) => {
    const targetDirectoryIndex = finalPath.findIndex(({ id }) => id === targetDirectory)
    const targetDirectoryInfo = finalPath[targetDirectoryIndex]

    if (targetDirectoryIndex === -1 || !targetDirectoryInfo) {
      return
    }

    const pathToDirectory = finalPath
      .slice(0, targetDirectoryIndex + 1)
      .map(({ id, categoryId }) => ({ id, categoryId }))

    const rootDirectoryInThePath = pathToDirectory.at(0)

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
      setExpandedDirectoryIds(pathToDirectory.map(({ id }) => id).concat(targetDirectory))
    }

    setSelectedAssets([
      {
        type: AssetType.directory,
        id: targetDirectory,
        parentId: pathToDirectory.at(-1)?.id ?? DirectoryId('directory-'),
        title: targetDirectoryInfo.label,
      },
    ])
  })

  const finalPath = (() => {
    const result: {
      id: DirectoryId
      categoryId: AnyCloudCategory['id'] | null
      label: AnyCloudCategory['label']
      icon: AnyCloudCategory['icon']
    }[] = []

    if (rootDirectoryInPath == null) {
      return result
    }

    const rootCategory = getCategoryByDirectoryId(rootDirectoryInPath)

    // If the root category is not found it might mean
    // that user is no longer have access to this root directory.
    // Usually this could happen if the user was removed from the organization
    // or user group.
    // This shouldn't happen though and these files should be filtered out
    // by the backend. But we need to handle this case anyway.
    if (rootCategory == null) {
      return result
    }

    result.push({
      id: rootDirectoryId,
      categoryId: rootCategory.id,
      label: rootCategory.label,
      icon: rootCategory.icon,
    })

    for (const [index, id] of virtualParentsIds.entries()) {
      const name = splitVirtualParentsPath.at(index)

      if (name == null) {
        continue
      }

      result.push({
        id,
        label: name,
        icon: FolderIcon,
        categoryId: null,
      })
    }

    return result
  })()

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

            <Text color="custom">{lastItemInPath.label}</Text>
          </div>
        </Button>

        <Popover
          size="auto"
          placement="bottom end"
          crossOffset={14}
          variant="light"
          className="max-w-lg"
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

/**
 * Props for the {@link PathItem} component.
 */
interface PathItemProps {
  readonly id: DirectoryId
  readonly label: AnyCloudCategory['label']
  readonly icon: AnyCloudCategory['icon']
  readonly onNavigate: (targetDirectory: DirectoryId) => void
}

/**
 * Individual item in the path.
 */
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
      {label}
    </Button>
  )
}
