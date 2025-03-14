/** @file Utilities related to `AssetRow`s. */
import type { AssetRowState } from '../../types'

/** The default {@link AssetRowState} associated with an `AssetRow`. */
export const INITIAL_ROW_STATE: AssetRowState = Object.freeze({
  setVisibility: () => {
    // Ignored. This MUST be replaced by the row component. It should also update `visibility`.
  },
  isEditingName: false,
  temporarilyAddedLabels: new Set(),
  temporarilyRemovedLabels: new Set(),
})
