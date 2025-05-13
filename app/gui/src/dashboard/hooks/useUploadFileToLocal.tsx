import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  type Category,
  useCategories,
  useTransferBetweenCategories,
} from '#/layouts/Drive/CategorySwitcher'
import { useText } from '#/providers/TextProvider'
import type { AnyAsset } from '#/services/Backend'
import invariant from 'tiny-invariant'
import { toast } from '../../../node_modules/react-toastify/dist/core/toast'

/**
 * Download a file to local.
 * Does not work in environments that do not have a local backend.
 */

export function useUploadFileToLocal(category: Category) {
  const { getText } = useText()
  const transferBetweenCategories = useTransferBetweenCategories(category)

  const { localCategories } = useCategories()
  const localHomeCategory = localCategories.categories.find(
    (otherCategory) => otherCategory.type === 'local',
  )
  return useEventCallback(async (assets: readonly AnyAsset[]) => {
    invariant(localHomeCategory, 'Local home category must exist to download to local')
    await transferBetweenCategories(category, localHomeCategory, assets)
    toast.success(getText('downloadProjectToLocalSuccess'))
  })
}
