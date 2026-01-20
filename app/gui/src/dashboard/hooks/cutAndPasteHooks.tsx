/** @file Events related to changes in the asset list. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  dropOperationBetweenCategories,
  useTransferBetweenCategories,
  type Category,
} from '#/layouts/Drive/Categories'
import type { DrivePastePayload } from '#/providers/DriveProvider'
import type { DirectoryId } from 'enso-common/src/services/Backend'
/**
 * Options for the paste action.
 */
export interface PasteActionOptions {
  readonly fromCategory: Category
  readonly toCategory: Category
  readonly newParentId: DirectoryId
  readonly pasteData: DrivePastePayload
  readonly method: 'copy' | 'move'
}

/**
 * A hook to copy or move assets as appropriate. Assets are moved, except when performing
 * a cut and paste between the Team Space and the User Space, in which case the asset is copied.
 */
export function usePaste(category: Category) {
  const transferBetweenCategories = useTransferBetweenCategories(category)

  return useEventCallback((options: PasteActionOptions) => {
    const { newParentId, pasteData, fromCategory, toCategory, method } = options
    const dropOperation = dropOperationBetweenCategories(fromCategory, toCategory, newParentId)
    if (dropOperation === 'cancel') return
    return transferBetweenCategories(
      fromCategory,
      toCategory,
      pasteData.assets,
      newParentId,
      method,
    )
  })
}
