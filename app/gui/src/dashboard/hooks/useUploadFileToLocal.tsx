/** @file A hook to download a file to local. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  type Category,
  useCategories,
  useTransferBetweenCategories,
} from '#/layouts/Drive/CategorySwitcher'
import type { AnyAsset } from '#/services/Backend'
import invariant from 'tiny-invariant'

/**
 * Download a file to local.
 * Does not work in environments that do not have a local backend.
 */
export function useUploadFileToLocal(category: Category) {
  const transferBetweenCategories = useTransferBetweenCategories(category)

  const { localCategories } = useCategories()
  const localHomeCategory = localCategories.categories.find(
    (otherCategory) => otherCategory.type === 'local',
  )
  return useEventCallback(async (assets: readonly AnyAsset[]) => {
    invariant(localHomeCategory, 'Local home category must exist to download to local')
    await transferBetweenCategories(category, localHomeCategory, assets)
  })
}
