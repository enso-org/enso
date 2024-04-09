/** @file Types related to pasting. */
import type PasteType from '#/utilities/PasteType'

// =================
// === PasteData ===
// =================

/** All information required to paste assets. */
export interface PasteData<T> {
  readonly type: PasteType
  readonly data: T
}
