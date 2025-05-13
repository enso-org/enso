/** @file Types for `CategorySwitcher`. */
import { CATEGORY_SCHEMA } from '#/layouts/Drive/CategorySwitcher/Category'
import { AssetType, type AssetId, type DirectoryId } from '#/services/Backend'
import { z } from 'zod'

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

/** A transferrable asset. */
export type TransferrableAsset = z.infer<typeof TRANSFERRABLE_ASSET_SCHEMA>

/** A data transfer payload for assets. */
export const ASSETS_DATA_TRANSFER_PAYLOAD = z.object({
  category: CATEGORY_SCHEMA,
  items: z.array(TRANSFERRABLE_ASSET_SCHEMA),
})

/** A data transfer payload for assets. */
export type AssetsDataTransferPayload = z.infer<typeof ASSETS_DATA_TRANSFER_PAYLOAD>
