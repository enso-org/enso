/** @file Events related to changes in the asset list. */
import { copyAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useTransferBetweenCategories, type Category } from '#/layouts/CategorySwitcher/Category'
import { useGetAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import type { DrivePastePayload } from '#/providers/DriveProvider'
import type Backend from '#/services/Backend'
import type { DirectoryId } from '#/services/Backend'
import { isTeamParentsPath, isUserParentsPath } from '#/utilities/permissions'
import { useMutation } from '@tanstack/react-query'

/**
 * A hook to copy or move assets as appropriate. Assets are moved, except when performing
 * a cut and paste between the Team Space and the User Space, in which case the asset is copied.
 */
export function useCutAndPaste(backend: Backend, category: Category) {
  const copyAssetsMutation = useMutation(copyAssetsMutationOptions(backend))
  const undoDeleteAssetMutation = useMutation(backendMutationOptions(backend, 'undoDeleteAsset'))
  const transferBetweenCategories = useTransferBetweenCategories(category)
  const getAsset = useGetAsset()

  return useEventCallback(async (newParentId: DirectoryId, pasteData: DrivePastePayload) => {
    const ids = Array.from(pasteData.ids)
    const assets = ids.flatMap((id) => {
      const item = getAsset(id)
      return item ? [item] : []
    })
    const newParent = getAsset(newParentId)
    const isMovingToUserSpace =
      newParent?.parentsPath != null && isUserParentsPath(newParent.ensoPath)
    if (pasteData.category.type === 'trash') {
      await Promise.all(
        assets.map((asset) =>
          undoDeleteAssetMutation.mutateAsync([asset.id, { parentId: newParentId }, asset.title]),
        ),
      )
    }
    const teamToUserItems =
      isMovingToUserSpace ? assets.filter((asset) => isTeamParentsPath(asset.ensoPath)) : []
    const nonTeamToUserIds =
      isMovingToUserSpace ?
        assets
          .filter((asset) => !isTeamParentsPath(asset.ensoPath))
          .map((otherItem) => otherItem.id)
      : ids
    if (teamToUserItems.length !== 0) {
      await copyAssetsMutation.mutateAsync([teamToUserItems.map((item) => item.id), newParentId])
    }
    if (nonTeamToUserIds.length !== 0) {
      transferBetweenCategories(pasteData.category, category, pasteData.ids, newParentId)
    }
  })
}
