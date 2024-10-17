/** @file Types related to pasting. */

/** The type of operation that should be triggered on paste. */
export type PasteType = 'copy' | 'move'

/** All information required to paste assets. */
export interface PasteData<T> {
  readonly type: PasteType
  readonly data: T
}
