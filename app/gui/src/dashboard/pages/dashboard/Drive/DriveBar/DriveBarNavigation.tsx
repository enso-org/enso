/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import BackIcon from '#/assets/expand_arrow_left.svg'
import * as React from 'react'

import { Button, ButtonGroup, Menu } from '#/components/AriaComponents'
import { Breadcrumbs, type OnDrop } from '#/components/Breadcrumbs'
import { Scroller } from '#/components/Scroller/Scroller'
import { moveAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { AssetPanelToggle } from '#/layouts/AssetPanel'
import { useCategories, useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useDriveStore } from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import { isDirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import { useState, useTransition } from 'react'
import { toast } from 'react-toastify'

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarNavigation() {
  const { getText } = useText()
  const { user } = useFullUserSession()
  const { getCategoryByDirectoryId } = useCategories()
  const { associatedBackend, category } = useCategoriesAPI()
  const [isTransitioning, startTransition] = useTransition()
  const [navigatingKey, setNavigatingKey] = useState<React.Key | null>(null)

  const { rootDirectoryId, currentDirectoryId, parentDirectoryId, setCurrentDirectoryId } =
    useDirectoryIds({ category })

  const driveStore = useDriveStore()

  const moveAssetsMutation = useMutation({
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
    ...listDirectoryQueryOptions({
      backend: associatedBackend,
      parentId: parentDirectoryId,
      category,
    }),
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
      }
    },
  })

  const navigateToDirectory = useEventCallback((id: React.Key) => {
    const parentId = finalPath.findIndex((item) => item.id === id) - 1

    if (!isDirectoryId(id)) {
      return
    }

    startTransition(() => {
      setNavigatingKey(id)
      setCurrentDirectoryId({
        current: id,
        // This is safe, because we know the index presents in the array.
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        parent: parentId < 0 ? null : finalPath[parentId]!.id,
      })
    })
  })

  const onDrop = useEventCallback<OnDrop>(async (id) => {
    const { selectedIds } = driveStore.getState()

    if (selectedIds.size === 0) {
      return
    }

    if (!isDirectoryId(id)) {
      return
    }

    await moveAssetsMutation.mutateAsync([[...selectedIds], id])
  })

  const navigateToParent = useEventCallback(() => {
    navigateToDirectory(parentDirectoryId)
  })

  const { finalPath } = parseDirectoriesPath({
    parentsPath: directoryData?.parentsPath ?? '',
    virtualParentsPath: directoryData?.virtualParentsPath ?? '',
    rootDirectoryId,
    getCategoryByDirectoryId,
  })

  switch (category.type) {
    case 'trash': {
      return (
        <div className="py-2">
          <Button
            icon={BackIcon}
            aria-label={getText('back')}
            isDisabled={currentDirectoryId === user.rootDirectoryId}
            onPress={navigateToParent}
            variant="icon"
          />
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
          <ButtonGroup className="mr-2 w-auto flex-none">
            <Menu.Trigger trigger="longPress">
              <Button
                icon={BackIcon}
                aria-label={getText('back')}
                isDisabled={currentDirectoryId === user.rootDirectoryId}
                onPress={navigateToParent}
                variant="icon"
              />

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
            <Breadcrumbs onAction={navigateToDirectory} onDrop={onDrop}>
              {finalPath.map((pathItem, index) => {
                const isLoading = isTransitioning && pathItem.id === navigatingKey
                return (
                  <Breadcrumbs.Item
                    key={pathItem.id + index}
                    id={pathItem.id}
                    icon={pathItem.icon}
                    isLoading={isLoading}
                  >
                    {pathItem.label}
                  </Breadcrumbs.Item>
                )
              })}
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
