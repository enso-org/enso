/** @file Utilities related to `AssetRow`s. */
import type * as assetsTable from '#/layouts/AssetsTable'

import * as set from '#/utilities/set'

/** The default {@link assetsTable.AssetRowState} associated with an `AssetRow`. */
export const INITIAL_ROW_STATE: assetsTable.AssetRowState = Object.freeze({
  setVisibility: () => {
    // Ignored. This MUST be replaced by the row component. It should also update `visibility`.
  },
  isEditingName: false,
  temporarilyAddedLabels: set.EMPTY_SET,
  temporarilyRemovedLabels: set.EMPTY_SET,
})
