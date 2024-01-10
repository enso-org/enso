/** @file Types related to pasting. */

// =================
// === PasteType ===
// =================

/** The type of operation that should be triggered on paste. */
export enum PasteType {
    copy = 'copy',
    move = 'move',
}

// =================
// === PasteData ===
// =================

/** All information required to paste assets. */
export interface PasteData<T> {
    type: PasteType
    data: T
}
