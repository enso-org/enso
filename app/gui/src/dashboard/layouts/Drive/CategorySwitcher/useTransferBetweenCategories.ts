/** @file A function to transfer a list of assets between categories. */
import { deleteAssetsMutationOptions, moveAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { useBackendQuery } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useFullUserSession } from '#/providers/AuthProvider'
import { useBackend, useLocalBackend, useRemoteBackend } from '#/providers/BackendProvider'
import type { AssetId, DirectoryId } from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import { useMutation } from '@tanstack/react-query'
import invariant from 'tiny-invariant'
import type { Category } from './Category'

/** A function to transfer a list of assets between categories. */
export function useTransferBetweenCategories(currentCategory: Category) {
  const remoteBackend = useRemoteBackend()
  const localBackend = useLocalBackend()
  const backend = useBackend(currentCategory)
  const { user } = useFullUserSession()
  const { data: organization = null } = useBackendQuery(remoteBackend, 'getOrganization', [])
  const deleteAssetsMutation = useMutation(deleteAssetsMutationOptions(backend))
  const moveAssetsMutation = useMutation(moveAssetsMutationOptions(backend))

  return useEventCallback(
    (from: Category, to: Category, keys: Iterable<AssetId>, newParentId?: DirectoryId | null) => {
      switch (from.type) {
        case 'cloud':
        case 'recent':
        case 'team':
        case 'user': {
          if (to.type === 'trash') {
            deleteAssetsMutation.mutate([[...keys], false])
          } else if (to.type === 'cloud' || to.type === 'team' || to.type === 'user') {
            newParentId ??=
              to.type === 'cloud' ?
                remoteBackend.rootDirectoryId(user, organization)
              : to.homeDirectoryId
            invariant(newParentId != null, 'The Cloud backend is missing a root directory.')
            moveAssetsMutation.mutate([[...keys], newParentId])
          }
          break
        }
        case 'trash': {
          break
        }
        case 'local':
        case 'local-directory': {
          if (to.type === 'local' || to.type === 'local-directory') {
            const parentDirectory = to.type === 'local' ? localBackend?.rootPath() : to.rootPath
            invariant(parentDirectory != null, 'The Local backend is missing a root directory.')
            newParentId ??= newDirectoryId(parentDirectory)
            moveAssetsMutation.mutate([[...keys], newParentId])
          }
        }
      }
    },
  )
}
