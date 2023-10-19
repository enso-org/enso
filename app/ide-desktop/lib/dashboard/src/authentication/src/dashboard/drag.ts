/** @file Various types of drag event payloads. */

import type * as backend from './backend'

// ============================
// === AssetRowsDragPayload ===
// ============================

export const ASSET_ROWS_DRAG_PAYLOAD_MIMETYPE = 'application/x-enso-asset-list'
const ASSET_ROWS_DRAG_PAYLOAD_MIMETYPE_REGEX = new RegExp(
    '^' + ASSET_ROWS_DRAG_PAYLOAD_MIMETYPE + '; id=(.+)$'
)
export const ASSET_ROWS_DRAG_PAYLOAD_MAP = new Map<string, AssetRowsDragPayload>()

/** Resolve to an {@link AssetRowsDragPayload}, if any, else resolve to `null`. */
export function tryGetAssetRowsDragPayload(dataTransfer: DataTransfer) {
    const item = Array.from(dataTransfer.items).find(dataTransferItem =>
        dataTransferItem.type.startsWith(ASSET_ROWS_DRAG_PAYLOAD_MIMETYPE)
    )
    const id = item?.type.match(ASSET_ROWS_DRAG_PAYLOAD_MIMETYPE_REGEX)?.[1] ?? null
    return id != null ? ASSET_ROWS_DRAG_PAYLOAD_MAP.get(id) ?? null : null
}

/** Metadata for an asset row. */
interface AssetRowsDragPayloadItem {
    key: backend.AssetId
    asset: backend.AnyAsset
}

/** Data for a {@link DragEvent} started from an {@link AssetsTable}. */
export type AssetRowsDragPayload = AssetRowsDragPayloadItem[]
