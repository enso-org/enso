/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import RecentIcon from '#/assets/recent.svg'
import { Breadcrumbs, type BreadcrumbItemProps, type OnDrop } from '#/components/Breadcrumbs'
import { Button } from '#/components/Button'
import { Popover } from '#/components/Dialog'
import { Menu } from '#/components/Menu'
import { Scroller } from '#/components/Scroller/Scroller'
import { moveAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import CategorySwitcher from '#/layouts/CategorySwitcher'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useCategories, useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import { useDriveStore } from '#/providers/DriveProvider'
import { AssetDoesNotExistError, isDirectoryId } from '#/services/Backend'
import type { PathItem } from '#/services/utilities'
import { parseDirectoriesPath } from '#/services/utilities'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useRightPanelData, useText } from '$/providers/react'
import { useSuspenseQuery } from '@tanstack/react-query'
import { useEffect, useTransition } from 'react'
import { toast } from 'react-toastify'

/** Props for a {@link DriveBarNavigation}. */
export interface DriveBarNavigationProps {
  readonly setCategoryId: (categoryId: Category['id']) => void
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarNavigation(props: DriveBarNavigationProps) {
  const { setCategoryId } = props

  const { getText } = useText()
  const { getCategoryByDirectoryId } = useCategories()
  const { associatedBackend, category } = useCategoriesAPI()

  const { rootDirectoryId, currentDirectoryId, setCurrentDirectoryId } = useDirectoryIds({
    category,
  })

  const rightPanel = useRightPanelData()

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

  const { data: directoryData } = useSuspenseQuery({
    queryKey: [associatedBackend.type, 'getAssetDetails', { id: currentDirectoryId }],
    queryFn: () => associatedBackend.getAssetDetails(currentDirectoryId),
    meta: { persist: false },
    retry: (count, error) => {
      if (error instanceof AssetDoesNotExistError) {
        setCurrentDirectoryId(null)
        return false
      }

      return count < 3
    },
    select: (data) => {
      if (data == null) {
        return null
      }

      const virtualParentsPath = () => {
        if (data.virtualParentsPath.length === 0) {
          return data.title
        }

        return data.virtualParentsPath + '/' + data.title
      }

      return {
        asset: data,
        parentsPath: data.parentsPath + '/' + data.id,
        virtualParentsPath: virtualParentsPath(),
        parentId: data.parentId,
      }
    },
  })

  useEffect(() => {
    if (directoryData?.asset != null) {
      rightPanel.updateContext('drive', (ctx) => {
        ctx.defaultItem = directoryData.asset
        return ctx
      })
    }
  }, [directoryData?.asset, rightPanel])

  const { finalPath: finalPathRaw } = parseDirectoriesPath({
    parentsPath: directoryData?.parentsPath ?? '',
    virtualParentsPath: directoryData?.virtualParentsPath ?? '',
    rootDirectoryId,
    getCategoryByDirectoryId,
  })
  const finalPath = (() => {
    if (category.type === 'recent') {
      return [
        {
          id: rootDirectoryId,
          categoryId: category.id,
          label: getText('recentCategory'),
          icon: RecentIcon,
        } satisfies PathItem,
        ...finalPathRaw.slice(1),
      ]
    }

    if (category.type === 'trash') {
      return [
        {
          id: category.homeDirectoryId,
          categoryId: category.id,
          label: getText('trashCategory'),
          icon: 'trash_small',
        } satisfies PathItem,
        ...finalPathRaw.slice(Math.max(1, finalPathRaw.length - 1)),
      ]
    }

    return finalPathRaw
  })()

  const parentId = finalPath.findIndex((item) => item.id === currentDirectoryId) - 1
  const canNavigateUp = parentId >= 0

  const setDirectoryId = useEventCallback((id: React.Key) => {
    if (!isDirectoryId(id)) {
      return
    }

    setCurrentDirectoryId(id)
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
    if (directoryData == null) {
      return
    }

    navigateToDirectory(directoryData.parentId)
  })

  const breadcrumbs = (
    <Scroller orientation="horizontal">
      <Breadcrumbs onDrop={onDrop}>
        {finalPath.map((pathItem, index) => {
          const isCurrent = pathItem.id === currentDirectoryId
          const breadcrumb = (
            <DriveBarBreadcrumbsItem
              key={pathItem.id + index}
              id={pathItem.id}
              icon={pathItem.icon}
              navigateToDirectory={navigateToDirectory}
              isDroppable={!isCurrent}
            >
              {pathItem.label}
            </DriveBarBreadcrumbsItem>
          )
          if (index === 0 && isCurrent) {
            return (
              <Menu.Trigger>
                <Button size="custom">
                  <DriveBarBreadcrumbsItem
                    key={pathItem.id + index}
                    id={pathItem.id}
                    icon={pathItem.icon}
                    navigateToDirectory={navigateToDirectory}
                    isDroppable={pathItem.id !== currentDirectoryId}
                  >
                    {pathItem.label}
                  </DriveBarBreadcrumbsItem>
                </Button>
                <Popover size="auto">
                  {({ close }) => {
                    return (
                      <CategorySwitcher
                        category={category}
                        setCategoryId={(id) => {
                          setCategoryId(id)
                          close()
                        }}
                      />
                    )
                  }}
                </Popover>
              </Menu.Trigger>
            )
          }
          return breadcrumb
        })}
      </Breadcrumbs>
    </Scroller>
  )

  switch (category.type) {
    case 'trash': {
      return (
        <div className="flex w-full flex-none items-center py-2">
          <UpButton navigateToParent={navigateToParent} isDisabled={!canNavigateUp} />
          {breadcrumbs}
        </div>
      )
    }
    case 'recent':
      return (
        <div className="flex w-full flex-none items-center py-2">
          <UpButton navigateToParent={navigateToParent} isDisabled />
          {breadcrumbs}
        </div>
      )
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <div className="flex w-full flex-none items-center">
          <Button.Group className="mr-2 w-auto flex-none" buttonVariants={{ variant: 'icon' }}>
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
          </Button.Group>

          {breadcrumbs}
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
    <Breadcrumbs.Item {...props} onPress={onPress} isLoading={isLoading} onDragDelay={onPress}>
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
