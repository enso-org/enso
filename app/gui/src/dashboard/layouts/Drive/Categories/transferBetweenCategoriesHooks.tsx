/** @file The categories available in the category switcher. */
import { Alert } from '#/components/Alert'
import { AlertDialog, ask, type Resolution } from '#/components/AlertDialog'
import { Text } from '#/components/Text'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  downloadAssetsMutationOptions,
  moveAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useUploadFileToCloud } from '#/hooks/backendUploadFilesHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import {
  CATEGORY_BACKEND,
  categoryEq,
  dropOperationBetweenCategories,
  isCloudCategory,
  isLocalCategory,
  type Category,
} from '$/providers/category'
import { useBackends, useCategories, useText, useUser } from '$/providers/react'
import type { GetText } from '$/providers/text'
import type { DropOperation } from '@react-types/shared'
import {
  AssetType,
  BackendType,
  type AssetId,
  type DirectoryId,
} from 'enso-common/src/services/Backend'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'
import { z } from 'zod'
import { parseDirectoriesPath } from './parseDirectoriesPath'

/** A transferrable asset. */
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

/** A data transfer payload for assets. */
export const ASSETS_DATA_TRANSFER_PAYLOAD = z.object({
  category: z.string(),
  items: z.array(TRANSFERRABLE_ASSET_SCHEMA),
})

/** A data transfer payload for assets. */
export type AssetsDataTransferPayload = z.infer<typeof ASSETS_DATA_TRANSFER_PAYLOAD>

/** A transferrable asset. */
export type TransferrableAsset = z.infer<typeof TRANSFERRABLE_ASSET_SCHEMA>

/** A signature of function returned from {@link useTransferBetweenCategories}. */
export type TransferBetweenCategoriesFunction = ReturnType<typeof useTransferBetweenCategories>

/** A function to transfer a list of assets between categories. */
export function useTransferBetweenCategories() {
  const { localBackend, remoteBackend } = useBackends()

  const { rootDirectoryId } = useUser()
  const { categoryDirectoryId, getCategoryByDirectoryId, categoryLabel } = useCategories()
  const { getText } = useText()

  const uploadFileToCloud = useUploadFileToCloud()
  const downloadAssets = useMutationCallback(downloadAssetsMutationOptions(remoteBackend))
  const deleteAssets = {
    [BackendType.local]: useMutationCallback(deleteAssetsMutationOptions(localBackend)),
    [BackendType.remote]: useMutationCallback(deleteAssetsMutationOptions(remoteBackend)),
  }
  const copyAssets = {
    [BackendType.local]: useMutationCallback(copyAssetsMutationOptions(localBackend)),
    [BackendType.remote]: useMutationCallback(copyAssetsMutationOptions(remoteBackend)),
  }
  const restoreAssets = useMutationCallback(restoreAssetsMutationOptions(remoteBackend))
  const moveAssets = {
    [BackendType.local]: useMutationCallback(moveAssetsMutationOptions(localBackend)),
    [BackendType.remote]: useMutationCallback(moveAssetsMutationOptions(remoteBackend)),
  }

  return useEventCallback(
    async (
      from: Category,
      to: Category,
      assets: Iterable<TransferrableAsset>,
      newParentId: DirectoryId | null = null,
      method: DropOperation = 'move',
    ) => {
      const operation = dropOperationBetweenCategories(from, to, newParentId)
      if (operation === 'cancel') return
      if (to.type === 'recent') return
      const assetsArray = Array.from(assets)
      const keysArray = assetsArray.map((asset) => asset.id)

      const targetDirectoryId = newParentId ?? categoryDirectoryId(to)
      if (targetDirectoryId == null) return

      const baseMutation =
        method === 'copy' ?
          () => copyAssets[CATEGORY_BACKEND[from.type]]([keysArray, targetDirectoryId])
        : method === 'move' ?
          () => moveAssets[CATEGORY_BACKEND[from.type]]([keysArray, targetDirectoryId])
        : () => {}

      switch (from.type) {
        case 'team':
        case 'cloud': {
          if (from.type === 'team' && to.type === 'team' && !categoryEq(from, to)) {
            let resolution: Resolution = 'confirm'

            if (method === 'move') {
              resolution = await askToCopyInstead(
                getText,
                getText('copyInsteadOfMoving', categoryLabel(from)),
              )
            }

            if (resolution === 'confirm') {
              await copyAssets[BackendType.remote]([keysArray, targetDirectoryId])
              return
            }
          }

          if (to.type === 'trash') {
            await deleteAssets[BackendType.remote]([keysArray, false])
            return
          }

          if (isLocalCategory(to)) {
            if (from.type === 'team' && method === 'move') {
              const resolution = await askToCopyInstead(
                getText,
                getText('copyInsteadOfMoving', categoryLabel(from)),
              )
              if (resolution !== 'confirm') {
                return
              }
            }

            await toast.promise(
              downloadAssets({
                ids: assetsArray,
                targetDirectoryId,
              }),
              {
                pending: getText('downloadingProjectToLocal'),
                success: getText('downloadProjectToLocalSuccess'),
                error: getText('downloadProjectToLocalError'),
              },
            )
            return
          }

          if (from.type === 'team' && to.type === 'cloud') {
            let resolution: Resolution = 'confirm'

            if (method === 'move') {
              resolution = await askToCopyInstead(
                getText,
                getText('copyInsteadOfMoving', categoryLabel(from)),
              )
            }

            if (resolution === 'confirm') {
              await copyAssets[BackendType.remote]([keysArray, targetDirectoryId])
              return
            }
          }

          baseMutation()
          return
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
            categoryLabel,
          )

          const entries = Array.from(groups.entries())

          return Promise.all([
            ...entries
              .filter(([category]) => category.type === 'cloud')
              .map(([_, assetsByCategory]) => {
                const assetsIds = assetsByCategory.map((asset) => asset.id)

                return restoreAssets({
                  ids: assetsIds,
                  parentId: targetDirectoryId,
                })
              }),
            ...entries
              .filter(([category]) => category.type === 'team')
              .map(async ([category, assetsByCategory]) => {
                const assetsIds = assetsByCategory.map((asset) => asset.id)

                const resolution = await ask(AlertDialog, {
                  title: getText('actionUnavailable'),
                  confirm: getText('copyInstead'),
                  children: (
                    <>
                      <Text>
                        {getText(
                          'copyInsteadOfRestoring',
                          categoryLabel(category),
                          categoryLabel(to),
                        )}
                      </Text>

                      <Alert variant="outline" icon="copy2">
                        <Text>
                          {getText(
                            'copyInsteadOfRestoringDescription',
                            categoryLabel(category),
                            categoryLabel(to),
                          )}
                        </Text>
                      </Alert>
                    </>
                  ),
                })

                if (resolution === 'confirm') {
                  return copyAssets[BackendType.remote]([assetsIds, targetDirectoryId])
                }
              }),
          ])
        }
        case 'local':
        case 'localDirectory': {
          invariant(
            localBackend != null,
            'The Local backend must be present to transfer assets from or to the local category.',
          )
          if (isCloudCategory(to)) {
            return uploadFileToCloud(localBackend, {
              assets: assetsArray,
              targetDirectoryId,
            })
          }
          baseMutation()
          return
        }
        case 'recent': {
          return
        }
      }
    },
  )
}

/** Groups transferrable assets by category. */
function groupTransferrableAssetsByCategory(
  assets: Iterable<TransferrableAsset>,
  rootDirectoryId: DirectoryId,
  getCategoryByDirectoryId: (directoryId: DirectoryId) => Category | undefined,
  categoryLabel: (category: Category) => string,
) {
  const groups = new Map<Category, TransferrableAsset[]>()

  for (const asset of assets) {
    const { category } = parseDirectoriesPath({
      parentsPath: asset.parentsPath,
      virtualParentsPath: asset.virtualParentsPath,
      rootDirectoryId,
      getCategoryByDirectoryId,
      categoryLabel,
    })

    if (category == null) {
      continue
    }

    groups.set(category, [...(groups.get(category) ?? []), asset])
  }

  return groups
}

/** Asks the user to copy instead of the operation. */
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
