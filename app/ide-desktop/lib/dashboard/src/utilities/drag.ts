/** @file Various types of drag event payloads. */
import type * as React from 'react'

import type * as backend from '#/services/Backend'

import * as uniqueString from '#/utilities/uniqueString'

// ===========================
// === setDragImageToBlank ===
// ===========================

/** Set the drag image to blank, so a custom div can be used instead. */
export function setDragImageToBlank(event: React.DragEvent) {
  const blankElement = document.createElement('div')
  const image = new Image()
  // Blank GIF
  image.src = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7'
  event.dataTransfer.setDragImage(image, 0, 0)
  blankElement.remove()
}

// ==========================
// === DragPayloadManager ===
// ==========================

/** Associates drag events with payload data. */
class DragPayloadManager<Payload> {
  regex: RegExp
  map = new Map<string, Payload>()
  reverseMap = new Map<Payload, string>()
  /** Create a {@link DragPayloadManager}. */
  constructor(public mimetype: string) {
    this.regex = new RegExp('^' + mimetype + '; id=(.+)$')
  }

  /** Tries to get the payload associated with a {@link React.DragEvent}. */
  lookup(event: React.DragEvent) {
    const item = Array.from(event.dataTransfer.items).find(dataTransferItem =>
      dataTransferItem.type.startsWith(this.mimetype)
    )
    const id = item?.type.match(this.regex)?.[1] ?? null
    return id != null ? this.map.get(id) ?? null : null
  }

  /** Associate data with a {@link React.DragEvent}. */
  bind(event: React.DragEvent, payload: Payload) {
    const id = uniqueString.uniqueString()
    event.dataTransfer.setData(`${this.mimetype}; id=${id}`, JSON.stringify(payload))
    this.map.set(id, payload)
    this.reverseMap.set(payload, id)
  }

  /** Dissociate data from its associated {@link React.DragEvent}. */
  unbind(payload: Payload) {
    const id = this.reverseMap.get(payload)
    this.reverseMap.delete(payload)
    if (id != null) {
      this.map.delete(id)
    }
  }
}

// ============================
// === AssetRowsDragPayload ===
// ============================

export const ASSET_ROWS = new DragPayloadManager<AssetRowsDragPayload>(
  'application/x-enso-asset-list'
)

/** Metadata for an asset row. */
interface AssetRowsDragPayloadItem {
  readonly key: backend.AssetId
  readonly asset: backend.AnyAsset
}

/** Data for a {@link DragEvent} started from an `AssetsTable`. */
export type AssetRowsDragPayload = readonly AssetRowsDragPayloadItem[]

// ========================
// === LabelDragPayload ===
// ========================

export const LABELS = new DragPayloadManager<LabelsDragPayload>('application/x-enso-label')

/** Data for a {@link DragEvent} started from an {@link backend.LabelName}. */
export type LabelsDragPayload = ReadonlySet<backend.LabelName>
