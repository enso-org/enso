/** @file A hook to return the asset tree. */
import { useMemo } from 'react'

import {
  useIsFetching,
  useQueries,
  useQuery,
  useQueryClient,
  useSuspenseQuery,
} from '@tanstack/react-query'

import type { DirectoryId } from 'enso-common/src/services/Backend'
import {
  BackendType,
  assetIsDirectory,
  createRootDirectoryAsset,
  createSpecialEmptyAsset,
  createSpecialErrorAsset,
  createSpecialLoadingAsset,
  type AnyAsset,
  type DirectoryAsset,
} from 'enso-common/src/services/Backend'

import { listDirectoryQueryOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useRefetchDirectories } from '#/layouts/Drive/fetchDirectoriesHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useBackend } from '#/providers/BackendProvider'
import AssetTreeNode, { type AnyAssetTreeNode } from '#/utilities/AssetTreeNode'

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

  // const directories = useQueries({
  //   // We query only expanded directories, as we don't want to load the data for directories that are not visible.
  //   queries: Array.from(new Set(expandedDirectoryIds)).map((directoryId) => ({
  //     ...listDirectoryQueryOptions({
  //       backend,
  //       parentId: directoryId,
  //       category,
  //     }),
  //   })),
  //   combine: (results) => {
  //     const rootQuery = results[expandedDirectoryIds.indexOf(rootDirectory.id)]

  //     return {
  //       rootDirectory: {
  //         isFetching: rootQuery?.isFetching ?? true,
  //         isLoading: rootQuery?.isLoading ?? true,
  //         isError: rootQuery?.isError ?? false,
  //         error: rootQuery?.error,
  //         data: rootQuery?.data,
  //       },
  //       directories: new Map(
  //         results.map((res, i) => [
  //           expandedDirectoryIds[i],
  //           {
  //             isFetching: res.isFetching,
  //             isLoading: res.isLoading,
  //             isError: res.isError,
  //             error: res.error,
  //             data: res.data,
  //           },
  //         ]),
  //       ),
  //     }
  //   },
  // })

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

  const assetTree = useMemo(() => {
    const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath(user)

    const children = expandedDirectoryData.map((content) => {
      const node = AssetTreeNode.fromAsset(content, 0, `${rootPath}/${content.title}`)
      return node
    })

    return new AssetTreeNode(rootDirectory, children, -1, rootPath)
  }, [backend, category, expandedDirectoryData, rootDirectory, user])

  return {
    assetTree,
    refetchDirectory,
  } as const
}
