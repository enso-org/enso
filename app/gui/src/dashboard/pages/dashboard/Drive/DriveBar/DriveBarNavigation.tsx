/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import { Button, ButtonGroup, Menu } from '#/components/AriaComponents'
import { Breadcrumbs, type BreadcrumbItemProps, type OnDrop } from '#/components/Breadcrumbs'
import { Scroller } from '#/components/Scroller/Scroller'
import { moveAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { AssetPanelToggle, useSetAssetPanelDefaultItem } from '#/layouts/AssetPanel'
import { useCategories, useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import { useDriveStore } from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import { isDirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useSuspenseQuery } from '@tanstack/react-query'
import { useEffect, useTransition } from 'react'
import { toast } from 'react-toastify'

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarNavigation() {
  const { getText } = useText()
  const { getCategoryByDirectoryId } = useCategories()
  const { associatedBackend, category } = useCategoriesAPI()

  const { rootDirectoryId, currentDirectoryId, parentDirectoryId, setCurrentDirectoryId } =
    useDirectoryIds({ category })

  const setAssetPanelDefaultItem = useSetAssetPanelDefaultItem()

  const driveStore = useDriveStore()

  const moveAssetsMutation = useMutationCallback({
    ...moveAssetsMutationOptions(associatedBackend),
    onSuccess: () => {
      driveStore.setState({ selectedIds: new Set(), visuallySelectedKeys: new Set() })
    },
    onError: (error) => {
      if ('failed' in error && error.failed !== 0) {
        toast.error(getText('moveMultipleAssetsError'))
        return
      }

      toast.error(getText('arbitraryMutationError'))
    },
  })

  const parentDirectoryQueryOptions = listDirectoryQueryOptions({
    backend: associatedBackend,
    parentId: parentDirectoryId,
    category,
    refetchInterval: null,
  })

  const { data: directoryData } = useSuspenseQuery({
    ...parentDirectoryQueryOptions,
    select: (data) => {
      if (parentDirectoryId === currentDirectoryId) {
        return null
      }

      const directory = data.find((item) => item.id === currentDirectoryId)

      if (directory == null) {
        return null
      }

      const virtualParentsPath = () => {
        if (directory.virtualParentsPath.length === 0) {
          return directory.title
        }

        return directory.virtualParentsPath + '/' + directory.title
      }

      return {
        parentsPath: directory.parentsPath + '/' + directory.id,
        virtualParentsPath: virtualParentsPath(),
        asset: directory,
      }
    },
  })

  useEffect(() => {
    setAssetPanelDefaultItem(directoryData?.asset ?? null)
  }, [directoryData?.asset, setAssetPanelDefaultItem])

  const { finalPath } = parseDirectoriesPath({
    parentsPath: directoryData?.parentsPath ?? '',
    virtualParentsPath: directoryData?.virtualParentsPath ?? '',
    rootDirectoryId,
    getCategoryByDirectoryId,
  })

  const parentId = finalPath.findIndex((item) => item.id === currentDirectoryId) - 1
  const canNavigateUp = parentId >= 0

  const setDirectoryId = useEventCallback((id: React.Key) => {
    const parentIdToNewId = finalPath.findIndex((item) => item.id === id) - 1

    if (!isDirectoryId(id)) {
      return
    }

    setCurrentDirectoryId({
      current: id,
      // This is safe, because we know the index presents in the array.
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      parent: parentIdToNewId < 0 ? null : finalPath[parentIdToNewId]!.id,
    })
  })

  const navigateToDirectory = useEventCallback((id: React.Key) => {
    setDirectoryId(id)
  })

  const onDrop = useEventCallback<OnDrop>(async (id) => {
    const { selectedIds } = driveStore.getState()

    if (selectedIds.size === 0) {
      return
    }

    if (!isDirectoryId(id)) {
      return
    }

    await moveAssetsMutation([[...selectedIds], id])
  })

  const navigateToParent = useEventCallback(() => {
    navigateToDirectory(parentDirectoryId)
  })

  switch (category.type) {
    case 'trash': {
      return (
        <div className="py-2">
          <UpButton navigateToParent={navigateToParent} isDisabled={!canNavigateUp} />
        </div>
      )
    }
    case 'recent':
      return null
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <div className="flex w-full flex-none items-center">
          <ButtonGroup className="mr-2 w-auto flex-none" buttonVariants={{ variant: 'icon' }}>
            <Menu.Trigger trigger="longPress">
              <UpButton navigateToParent={navigateToParent} isDisabled={!canNavigateUp} />

              <Menu items={[...finalPath].reverse()} onAction={navigateToDirectory}>
                {(item) => {
                  const index = finalPath.findIndex((pathItem) => pathItem.id === item.id)
                  return (
                    <Menu.Item
                      key={item.id + index}
                      id={item.id}
                      icon={item.icon}
                      isDisabled={item.id === currentDirectoryId}
                    >
                      {item.label}
                    </Menu.Item>
                  )
                }}
              </Menu>
            </Menu.Trigger>
          </ButtonGroup>

          <Scroller orientation="horizontal">
            <Breadcrumbs onDrop={onDrop}>
              {finalPath.map((pathItem, index) => (
                <DriveBarBreadcrumbsItem
                  key={pathItem.id + index}
                  id={pathItem.id}
                  icon={pathItem.icon}
                  navigateToDirectory={navigateToDirectory}
                  isDroppable={pathItem.id !== currentDirectoryId}
                >
                  {pathItem.label}
                </DriveBarBreadcrumbsItem>
              ))}
            </Breadcrumbs>
          </Scroller>

          <div className="ml-auto">
            <AssetPanelToggle showWhen="collapsed" className="my-auto" />
          </div>
        </div>
      )
    }
  }
}

/** Props for {@link DriveBarBreadcrumbsItem}. */
interface DriveBarBreadcrumbsItemProps<IconType extends string>
  extends BreadcrumbItemProps<IconType> {
  readonly navigateToDirectory: (id: React.Key) => void
}

/** A breadcrumb item for the drive bar. */
function DriveBarBreadcrumbsItem<IconType extends string>(
  props: DriveBarBreadcrumbsItemProps<IconType>,
) {
  const { navigateToDirectory } = props

  const [isLoading, startTransition] = useTransition()

  const onPress = useEventCallback(() => {
    const id = props.id

    if (id == null) {
      return
    }

    startTransition(() => {
      navigateToDirectory(id)
    })
  })

  return (
    <Breadcrumbs.Item {...props} onPress={onPress} isLoading={isLoading}>
      {props.children}
    </Breadcrumbs.Item>
  )
}

/** Props for {@link UpButton}. */
interface UpButtonProps {
  readonly navigateToParent: () => void
  readonly isDisabled: boolean
}

/** A button for navigating to the parent directory. */
function UpButton(props: UpButtonProps) {
  const { navigateToParent, isDisabled } = props
  const { getText } = useText()

  const [isLoading, startTransition] = useTransition()

  const onPress = useEventCallback(() => {
    startTransition(() => {
      navigateToParent()
    })
  })

  return (
    <Button
      variant="icon"
      icon="navigate_up"
      aria-label={getText('up')}
      onPress={onPress}
      isLoading={isLoading}
      isDisabled={isDisabled}
    />
  )
}
