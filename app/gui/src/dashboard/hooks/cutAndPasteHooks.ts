/** @file Events related to changes in the asset list. */
import { copyAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useTransferBetweenCategories, type Category } from '#/layouts/CategorySwitcher/Category'
import type { DrivePastePayload } from '#/providers/DriveProvider'
import type Backend from '#/services/Backend'
import type { AssetId, DirectoryId } from '#/services/Backend'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import { isTeamPath, isUserPath } from '#/utilities/permissions'
import { useMutation } from '@tanstack/react-query'

/**
 * A hook to copy or move assets as appropriate. Assets are moved, except when performing
 * a cut and paste between the Team Space and the User Space, in which case the asset is copied.
 */
export function useCutAndPaste(backend: Backend, category: Category) {
  const copyAssetsMutation = useMutation(copyAssetsMutationOptions(backend))
  const transferBetweenCategories = useTransferBetweenCategories(category)

  return useEventCallback(
    (
      newParentKey: DirectoryId,
      newParentId: DirectoryId,
      pasteData: DrivePastePayload,
      nodeMap: ReadonlyMap<AssetId, AnyAssetTreeNode>,
    ) => {
      const ids = Array.from(pasteData.ids)
      const nodes = ids.flatMap((id) => {
        const item = nodeMap.get(id)
        return item == null ? [] : [item]
      })
      const newParent = nodeMap.get(newParentKey)
      const isMovingToUserSpace = newParent?.path != null && isUserPath(newParent.path)
      const teamToUserItems =
        isMovingToUserSpace ?
          nodes.filter((node) => isTeamPath(node.path)).map((otherItem) => otherItem.item)
        : []
      const nonTeamToUserIds =
        isMovingToUserSpace ?
          nodes.filter((node) => !isTeamPath(node.path)).map((otherItem) => otherItem.item.id)
        : ids
      if (teamToUserItems.length !== 0) {
        copyAssetsMutation.mutate([teamToUserItems.map((item) => item.id), newParentId])
      }
      if (nonTeamToUserIds.length !== 0) {
        transferBetweenCategories(pasteData.category, category, pasteData.ids, newParentId)
      }
    },
  )
}
