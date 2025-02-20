/** @file A hook to return the asset tree. */
import { useMemo } from 'react'

import { useIsFetching, useQueries, useQueryClient } from '@tanstack/react-query'

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
  readonly hidden: boolean
  readonly category: Category
  readonly rootDirectory: DirectoryAsset
  readonly expandedDirectoryIds: readonly DirectoryId[]
}

/** A hook to return the asset tree. */
export function useAssetTree(options: UseAssetTreeOptions) {
  const { hidden, category, rootDirectory, expandedDirectoryIds } = options

  const { user } = useFullUserSession()

  const backend = useBackend(category)

  useRefetchDirectories(BackendType.local)
  useRefetchDirectories(BackendType.remote)

  const directories = useQueries({
    // We query only expanded directories, as we don't want to load the data for directories that are not visible.
    queries: expandedDirectoryIds.map((directoryId) => ({
      ...listDirectoryQueryOptions({
        backend,
        parentId: directoryId,
        category,
      }),
      enabled: !hidden,
    })),
    combine: (results) => {
      const rootQuery = results[expandedDirectoryIds.indexOf(rootDirectory.id)]

      return {
        rootDirectory: {
          isFetching: rootQuery?.isFetching ?? true,
          isLoading: rootQuery?.isLoading ?? true,
          isError: rootQuery?.isError ?? false,
          error: rootQuery?.error,
          data: rootQuery?.data,
        },
        directories: new Map(
          results.map((res, i) => [
            expandedDirectoryIds[i],
            {
              isFetching: res.isFetching,
              isLoading: res.isLoading,
              isError: res.isError,
              error: res.error,
              data: res.data,
            },
          ]),
        ),
      }
    },
  })

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

  const isFetching = useIsFetching({
    queryKey: [backend.type, 'listDirectory'],
  })

  const rootDirectoryContent = directories.rootDirectory.data
  const isError = directories.rootDirectory.isError
  const isLoading = directories.rootDirectory.isLoading && !isError

  const assetTree = useMemo(() => {
    const rootPath = 'rootPath' in category ? category.rootPath : backend.rootPath(user)

    // If the root directory is not loaded, then we cannot render the tree.
    // Return null, and wait for the root directory to load.
    if (rootDirectoryContent == null) {
      return AssetTreeNode.fromAsset(createRootDirectoryAsset(rootDirectory.id), -1, rootPath)
    } else if (isError) {
      return AssetTreeNode.fromAsset(createRootDirectoryAsset(rootDirectory.id), -1, rootPath).with(
        {
          children: [AssetTreeNode.fromAsset(createSpecialErrorAsset(rootDirectory.id), 0, '')],
        },
      )
    }

    const children = rootDirectoryContent.map((content) => {
      /**
       * Recursively build assets tree. If a child is a directory, we search for its content
       * in the loaded data. If it is loaded, we append that data to the asset node
       * and do the same for the children.
       */
      const withChildren = (node: AnyAssetTreeNode, depth: number): AnyAssetTreeNode => {
        const { item } = node

        if (assetIsDirectory(item)) {
          const childrenAssetsQuery = directories.directories.get(item.id)

          const nestedChildren = childrenAssetsQuery?.data?.map((child) =>
            AssetTreeNode.fromAsset(child, depth, `${node.path}/${child.title}`),
          )

          if (childrenAssetsQuery == null || childrenAssetsQuery.isLoading) {
            return node.with({
              children: [AssetTreeNode.fromAsset(createSpecialLoadingAsset(item.id), depth, '')],
            })
          } else if (childrenAssetsQuery.isError) {
            return node.with({
              children: [AssetTreeNode.fromAsset(createSpecialErrorAsset(item.id), depth, '')],
            })
          } else if (nestedChildren?.length === 0) {
            return node.with({
              children: [AssetTreeNode.fromAsset(createSpecialEmptyAsset(item.id), depth, '')],
            })
          } else if (nestedChildren != null) {
            return node.with({
              children: nestedChildren.map((child) => withChildren(child, depth + 1)),
            })
          }
        }

        return node
      }

      const node = AssetTreeNode.fromAsset(content, 0, `${rootPath}/${content.title}`)

      const ret = withChildren(node, 1)
      return ret
    })

    return new AssetTreeNode(rootDirectory, children, -1, rootPath)
  }, [
    backend,
    category,
    directories.directories,
    isError,
    rootDirectory,
    rootDirectoryContent,
    user,
  ])

  return {
    isLoading,
    isError,
    assetTree,
    isFetching,
    refetchDirectory,
  } as const
}
