/** @file The categories available in the category switcher. */
import invariant from 'tiny-invariant'

import { Alert, AlertDialog, ask, Text } from '#/components/AriaComponents'
import {
  copyAssetsMutationOptions,
  deleteAssetsMutationOptions,
  moveAssetsMutationOptions,
  restoreAssetsMutationOptions,
} from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useUser } from '#/providers/AuthProvider'
import { useBackend, useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { AssetType, type AssetId, type DirectoryId } from '#/services/Backend'
import { parseDirectoriesPath } from '#/services/utilities'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import type { DropOperation } from '@react-types/shared'
import { z } from 'zod'
import {
  CATEGORY_SCHEMA,
  dropOperationBetweenCategories,
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
  const backend = useBackend(currentCategory)

  const { rootDirectoryId } = useUser()

  const { getCategoryByDirectoryId } = useCategories()

  const { getText } = useText()

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
      const keysArray = Array.from(assets).map((asset) => asset.id)

      if (operation === 'cancel') {
        return
      }

      if (to.type === 'recent') {
        return
      }

      switch (from.type) {
        case 'team': {
          if (to.type === 'trash') {
            return deleteAssetsMutation([keysArray, false])
          }

          if (to.type === 'cloud' || to.type === 'user') {
            return ask(AlertDialog, {
              title: getText('actionUnavailable'),
              confirm: getText('copyInstead'),
              children: (
                <>
                  <Text>{getText('copyInsteadOfMoving', from.label)}</Text>

                  <Alert variant="outline" icon="copy2">
                    <Text>{getText('youCanCopyInstead')}</Text>
                  </Alert>
                </>
              ),
            }).then((resolution) => {
              if (resolution === 'confirm') {
                return copyAssetsMutation([keysArray, newParentId ?? to.homeDirectoryId])
              }
            })
          }

          return mutationByOperation[method](keysArray, newParentId ?? to.homeDirectoryId)
        }
        case 'cloud':
        case 'user': {
          if (to.type === 'trash') {
            return deleteAssetsMutation([keysArray, false])
          }

          return mutationByOperation[method](keysArray, newParentId ?? to.homeDirectoryId)
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
              .map(([category, assetsByCategory]) => {
                const assetsIds = assetsByCategory.map((asset) => asset.id)

                return restoreAssetsMutation({
                  ids: assetsIds,
                  parentId: newParentId ?? category.homeDirectoryId,
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
                    return copyAssetsMutation([assetsIds, newParentId ?? to.homeDirectoryId])
                  }
                })
              }),
          ])
        }
        case 'local':
        case 'local-directory': {
          if (to.type === 'local' || to.type === 'local-directory') {
            const parentDirectory = to.type === 'local' ? localBackend?.rootPath() : to.rootPath

            invariant(parentDirectory != null, 'The Local backend is missing a root directory.')

            return mutationByOperation[method](keysArray, newParentId ?? to.homeDirectoryId)
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
