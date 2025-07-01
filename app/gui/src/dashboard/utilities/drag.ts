/** @file Various types of drag event payloads. */
import type { Category } from '#/layouts/Drive/Categories/Category'
import type { AnyAsset, AssetId } from '#/services/Backend'

/** Metadata for an asset row. */
interface AssetRowsDragPayloadItem {
  readonly key: AssetId
  readonly asset: AnyAsset
}

/** Data for a {@link DragEvent} started from an `AssetsTable`. */
export interface AssetRowsDragPayload {
  readonly category: Category
  readonly items: readonly AssetRowsDragPayloadItem[]
}
