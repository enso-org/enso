/**
 * @file Header menubar for the directory listing, containing information about
 * the current directory and some configuration options.
 */
import BackIcon from '#/assets/expand_arrow_left.svg'
import * as React from 'react'

import { Button, ButtonGroup } from '#/components/AriaComponents'
import { Breadcrumbs, type OnDrop } from '#/components/Breadcrumbs'
import { Scroller } from '#/components/Scroller/Scroller'
import { moveAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { AssetPanelToggle } from '#/layouts/AssetPanel'
import { type Category } from '#/layouts/CategorySwitcher/Category'
import { useCategories, useCategoriesAPI } from '#/layouts/Drive/Categories/categoriesHooks'
import { useDirectoryIds } from '#/layouts/Drive/directoryIdsHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useDriveStore } from '#/providers/DriveProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { isDirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { getMessageOrToString } from '#/utilities/error'
import { useMutation, useSuspenseQuery } from '@tanstack/react-query'
import { useState, useTransition } from 'react'
import { toast } from 'react-toastify'

/** Props for a {@link DriveBarNavigation}. */
export interface DriveBarNavigationProps {
  readonly backend: Backend
  readonly category: Category
}

/**
 * Displays the current directory path and permissions, upload and download buttons,
 * and a column display mode switcher.
 */
export function DriveBarNavigation(props: DriveBarNavigationProps) {
  const { backend, category } = props

  const { getText } = useText()
  const { user } = useFullUserSession()
  const { getCategoryByDirectoryId } = useCategories()
  const { associatedBackend } = useCategoriesAPI()
  const [isTransitioning, startTransition] = useTransition()
  const [navigatingKey, setNavigatingKey] = useState<React.Key | null>(null)

  const { rootDirectoryId, currentDirectoryId, parentDirectoryId, setCurrentDirectoryId } =
    useDirectoryIds({ category })

  const driveStore = useDriveStore()

  const moveAssetsMutation = useMutation({
    ...moveAssetsMutationOptions(associatedBackend),
    onSuccess: () => {
      driveStore.setState({ selectedKeys: new Set(), visuallySelectedKeys: new Set() })
    },
    onError: (error) => {
      if ('errors' in error && Array.isArray(error.errors)) {
        for (const message of error.errors) {
          toast.error(getMessageOrToString(message))
        }

        return
      }

      toast.error(getText('arbitraryMutationError'))
    },
  })

  const { data: directoryData } = useSuspenseQuery({
    ...listDirectoryQueryOptions({
      backend,
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
        parentsPath: directory.parentsPath + '/' + currentDirectoryId,
        virtualParentsPath: virtualParentsPath(),
      }
    },
  })

  const navigateToDirectory = useEventCallback((id: React.Key) => {
    const parentId = finalPath.findIndex((item) => item.id === id) - 1

    if (!isDirectoryId(id)) {
      return
    }

    setNavigatingKey(id)
    startTransition(() => {
      setCurrentDirectoryId({
        current: id,
        // This is safe, because we know the index is present in the array.
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        parent: parentId < 0 ? null : finalPath[parentId]!.id,
      })
    })
  })

  const onDrop = useEventCallback<OnDrop>(async (id, e) => {
    const { selectedKeys } = driveStore.getState()

    if (selectedKeys.size === 0) {
      return
    }

    if (!isDirectoryId(id)) {
      return
    }

    await moveAssetsMutation.mutateAsync([[...selectedKeys], id])
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
    case 'recent':
    case 'trash': {
      return null
    }
    case 'cloud':
    case 'local':
    case 'user':
    case 'team':
    case 'local-directory': {
      return (
        <div className="flex w-full max-w-fit flex-none  items-center">
          <ButtonGroup
            className="mr-4 w-auto flex-none"
            buttonVariants={{ variant: 'icon', size: 'small' }}
          >
            <Button
              icon={BackIcon}
              aria-label={getText('back')}
              isDisabled={currentDirectoryId === user.rootDirectoryId}
              onPress={navigateToParent}
            />
          </ButtonGroup>

          <Scroller>
            <Breadcrumbs onAction={navigateToDirectory} onDrop={onDrop}>
              {finalPath.map((pathItem) => {
                const isLoading = isTransitioning && pathItem.id === navigatingKey
                return (
                  <Breadcrumbs.Item
                    key={pathItem.id}
                    id={pathItem.id}
                    icon={pathItem.icon}
                    isLoading={isLoading}
                    className="snap-start"
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
