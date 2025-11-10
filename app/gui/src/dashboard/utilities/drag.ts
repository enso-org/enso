/** @file Various types of drag event payloads. */
import type { AnyAsset, AssetId } from 'enso-common/src/services/Backend'
import type { Category } from 'enso-common/src/services/Backend/Category'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import type { DragEvent } from 'react'

/** Set the drag image to blank, so a custom div can be used instead. */
export function setDragImageToBlank(event: DragEvent) {
  const blankElement = document.createElement('div')
  const image = new Image()
  // Blank GIF
  image.src = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7'
  event.dataTransfer.setDragImage(image, 0, 0)
  blankElement.remove()
}

/** Associates drag events with payload data. */
class DragPayloadManager<Payload> {
  readonly regex: RegExp
  readonly map = new Map<string, Payload>()
  readonly reverseMap = new Map<Payload, string>()
  /** Create a {@link DragPayloadManager}. */
  constructor(public mimetype: string) {
    this.regex = new RegExp('^' + mimetype + '; id=(.+)$')
  }

  /** Tries to get the payload associated with a {@link DragEvent}. */
  lookup(event: DragEvent) {
    const item = Array.from(event.dataTransfer.items).find((dataTransferItem) =>
      dataTransferItem.type.startsWith(this.mimetype),
    )
    const id = item?.type.match(this.regex)?.[1] ?? null
    return id != null ? (this.map.get(id) ?? null) : null
  }

  /** Associate data with a {@link DragEvent}. */
  bind(event: DragEvent, payload: Payload) {
    const id = uniqueString()
    event.dataTransfer.setData(`${this.mimetype}; id=${id}`, JSON.stringify(payload))
    this.map.set(id, payload)
    this.reverseMap.set(payload, id)
  }

  /** Dissociate data from its associated {@link DragEvent}. */
  unbind(payload: Payload) {
    const id = this.reverseMap.get(payload)
    this.reverseMap.delete(payload)
    if (id != null) {
      this.map.delete(id)
    }
  }
}

export const ASSET_ROWS = new DragPayloadManager<AssetRowsDragPayload>(
  'application/x-enso-asset-list',
)

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
