/** @file A hook to return the asset tree. */
import { useQueryClient, useSuspenseQuery } from '@tanstack/react-query'

import type { DirectoryId } from 'enso-common/src/services/Backend'
import { BackendType, type AnyAsset, type DirectoryAsset } from 'enso-common/src/services/Backend'

import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useRefetchDirectories } from '#/layouts/Drive/fetchDirectoriesHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useBackend } from '#/providers/BackendProvider'
import AssetTreeNode from '#/utilities/AssetTreeNode'

/** Return type of the query function for the `listDirectory` query. */
export type DirectoryQuery = readonly AnyAsset[] | undefined

/** Options for {@link useAssetTree}. */
export interface UseAssetTreeOptions {
  readonly category: Category
  readonly rootDirectory: DirectoryAsset
  readonly expandedDirectoryId: DirectoryId
}

/** A hook to return the asset tree. */
export function useAssetTree(options: UseAssetTreeOptions) {
  const { category, rootDirectory, expandedDirectoryId } = options

  const { user } = useFullUserSession()

  const backend = useBackend(category)

  useRefetchDirectories(BackendType.local)
  useRefetchDirectories(BackendType.remote)

  const { data: expandedDirectoryData } = useSuspenseQuery(
    listDirectoryQueryOptions({ backend, parentId: expandedDirectoryId, category }),
  )

  const queryClient = useQueryClient()

  /** Refetch the directory data for a given directory. */
  const refetchDirectory = useEventCallback((directoryId: DirectoryId) => {
    return queryClient.refetchQueries({
      queryKey: listDirectoryQueryOptions({
        backend,
        parentId: directoryId,
        category,
      }).queryKey,
      type: 'active',
    })
  })

  const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath(user)

  const assetTree = new AssetTreeNode(
    rootDirectory,
    expandedDirectoryData.map((content) =>
      AssetTreeNode.fromAsset(content, 0, `${rootPath}/${content.title}`),
    ),
    -1,
    rootPath,
  )

  return {
    assetTree,
    refetchDirectory,
  } as const
}
