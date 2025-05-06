/** @file The categories available in the category switcher. */
import invariant from 'tiny-invariant'

import type { Resolution } from '#/components/AriaComponents'
import { Alert, AlertDialog, ask, Text } from '#/components/AriaComponents'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  moveAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useUploadFileToCloudMutation } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useUser } from '#/providers/AuthProvider'
import { useBackend, useLocalBackend, useRemoteBackend } from '#/providers/BackendProvider'
import { useText, type GetText } from '#/providers/TextProvider'
import { AssetType, type AssetId, type DirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import type { DropOperation } from '@react-types/shared'
import { z } from 'zod'
import {
  CATEGORY_SCHEMA,
  dropOperationBetweenCategories,
  isCloudCategory,
  isLocalCategory,
  type Category,
} from './Category'
import { useCategories } from './categoriesHooks'

/**
 * A transferrable asset.
 */
export const TRANSFERRABLE_ASSET_SCHEMA = z.object({
  // eslint-disable-next-line no-restricted-syntax
  id: z.string().transform((id) => id as AssetId),
  title: z.string(),
  type: z.nativeEnum(AssetType),
  // eslint-disable-next-line no-restricted-syntax
  parentId: z.string().transform((id) => id as DirectoryId),
  parentsPath: z.string(),
  virtualParentsPath: z.string(),
})

/**
 * A data transfer payload for assets.
 */
export const ASSETS_DATA_TRANSFER_PAYLOAD = z.object({
  category: CATEGORY_SCHEMA,
  items: z.array(TRANSFERRABLE_ASSET_SCHEMA),
})

/**
 * A data transfer payload for assets.
 */
export type AssetsDataTransferPayload = z.infer<typeof ASSETS_DATA_TRANSFER_PAYLOAD>

/**
 * A transferrable asset.
 */
export type TransferrableAsset = z.infer<typeof TRANSFERRABLE_ASSET_SCHEMA>

/** A function to transfer a list of assets between categories. */
export function useTransferBetweenCategories(currentCategory: Category) {
  const localBackend = useLocalBackend()
  const remoteBackend = useRemoteBackend()
  const backend = useBackend(currentCategory)

  const { rootDirectoryId } = useUser()

  const { getCategoryByDirectoryId } = useCategories()

  const { getText } = useText()

  const uploadFileToCloudMutation = useUploadFileToCloudMutation()
  const downloadAssetsMutation = useMutationCallback(downloadAssetsMutationOptions(remoteBackend))
  const deleteAssetsMutation = useMutationCallback(deleteAssetsMutationOptions(backend))
  const copyAssetsMutation = useMutationCallback(copyAssetsMutationOptions(backend))
  const restoreAssetsMutation = useMutationCallback(restoreAssetsMutationOptions(backend))
  const moveAssetsMutation = useMutationCallback(moveAssetsMutationOptions(backend))

  const mutationByOperation = {
    cancel: () => Promise.resolve(),
    move: (keys: Array<AssetId>, newParentId: DirectoryId) =>
      moveAssetsMutation([keys, newParentId]),
    copy: (keys: Array<AssetId>, newParentId: DirectoryId) =>
      copyAssetsMutation([keys, newParentId]),
    link: () => Promise.resolve(),
  } as const

  return useEventCallback(
    async (
      from: Category,
      to: Category,
      assets: Iterable<TransferrableAsset>,
      newParentId: DirectoryId | null = null,
      method: DropOperation = 'move',
    ) => {
      const operation = dropOperationBetweenCategories(from, to, newParentId)

      if (operation === 'cancel') {
        return
      }

      if (to.type === 'recent') {
        return
      }

      const assetsArray = Array.from(assets)
      const keysArray = assetsArray.map((asset) => asset.id)
      const targetDirectoryId = newParentId ?? to.homeDirectoryId

      switch (from.type) {
        case 'team': {
          if (to.type === 'trash') {
            return deleteAssetsMutation([keysArray, false])
          }

          if (isLocalCategory(to)) {
            if (method === 'move') {
              return askToCopyInstead(getText, getText('copyInsteadOfMoving', from.label))
            }

            return downloadAssetsMutation({
              ids: assetsArray,
              targetDirectoryId,
            })
          }

          if (to.type === 'cloud' || to.type === 'user') {
            let resolution: Resolution = 'confirm'

            if (method === 'move') {
              resolution = await askToCopyInstead(
                getText,
                getText('copyInsteadOfMoving', from.label),
              )
            }

            if (resolution === 'confirm') {
              return copyAssetsMutation([keysArray, targetDirectoryId])
            }
          }

          return mutationByOperation[method](keysArray, targetDirectoryId)
        }
        case 'cloud':
        case 'user': {
          if (to.type === 'trash') {
            return deleteAssetsMutation([keysArray, false])
          }

          if (isLocalCategory(to)) {
            return downloadAssetsMutation({
              ids: assetsArray,
              targetDirectoryId: newParentId ?? to.homeDirectoryId,
            })
          }

          return mutationByOperation[method](keysArray, targetDirectoryId)
        }
        case 'trash': {
          if (to.type === 'trash') {
            return
          }

          if (isLocalCategory(to)) {
            return
          }

          const groups = groupTransferrableAssetsByCategory(
            assets,
            rootDirectoryId,
            getCategoryByDirectoryId,
          )

          const entries = Array.from(groups.entries())

          return Promise.all([
            ...entries
              .filter(([category]) => category.type === 'user' || category.type === 'cloud')
              .map(([_, assetsByCategory]) => {
                const assetsIds = assetsByCategory.map((asset) => asset.id)

                return restoreAssetsMutation({
                  ids: assetsIds,
                  parentId: targetDirectoryId,
                })
              }),
            ...entries
              .filter(([category]) => category.type === 'team')
              .map(([category, assetsByCategory]) => {
                const assetsIds = assetsByCategory.map((asset) => asset.id)

                return ask(AlertDialog, {
                  title: getText('actionUnavailable'),
                  confirm: getText('copyInstead'),
                  children: (
                    <>
                      <Text>{getText('copyInsteadOfRestoring', category.label, to.label)}</Text>

                      <Alert variant="outline" icon="copy2">
                        <Text>
                          {getText('copyInsteadOfRestoringDescription', category.label, to.label)}
                        </Text>
                      </Alert>
                    </>
                  ),
                }).then((resolution) => {
                  if (resolution === 'confirm') {
                    return copyAssetsMutation([assetsIds, targetDirectoryId])
                  }
                })
              }),
          ])
        }
        case 'local':
        case 'local-directory': {
          invariant(
            localBackend != null,
            'The Local backend must be present to transfer assets from or to the local category.',
          )

          if (isCloudCategory(to)) {
            return uploadFileToCloudMutation(localBackend, {
              assets: assetsArray,
              targetDirectoryId,
            })
          }

          if (to.type === 'local') {
            return mutationByOperation[method](keysArray, targetDirectoryId)
          }

          return
        }
        case 'recent': {
          return
        }
      }
    },
  )
}

/**
 * Groups transferrable assets by category.
 */
function groupTransferrableAssetsByCategory(
  assets: Iterable<TransferrableAsset>,
  rootDirectoryId: DirectoryId,
  getCategoryByDirectoryId: (directoryId: DirectoryId) => Category | null,
) {
  const groups = new Map<Category, TransferrableAsset[]>()

  for (const asset of assets) {
    const { category } = parseDirectoriesPath({
      parentsPath: asset.parentsPath,
      virtualParentsPath: asset.virtualParentsPath,
      rootDirectoryId,
      getCategoryByDirectoryId,
    })

    if (category == null) {
      continue
    }

    groups.set(category, [...(groups.get(category) ?? []), asset])
  }

  return groups
}

/**
 * Asks the user to copy instead of the operation.
 */
function askToCopyInstead(getText: GetText, text: string) {
  return ask(AlertDialog, {
    title: getText('actionUnavailable'),
    confirm: getText('copyInstead'),
    children: (
      <>
        <Text>{text}</Text>

        <Alert variant="outline" icon="copy2">
          <Text>{getText('youCanCopyInstead')}</Text>
        </Alert>
      </>
    ),
  })
}
