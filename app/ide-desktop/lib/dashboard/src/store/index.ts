/** @file A bounded store composed of all the slices used in this application. */
import * as zustand from 'zustand'

import * as assetsTableSlice from '#/store/assetsTableSlice'

// =================
// === Constants ===
// =================

/** The default {@link AssetState} for an arbitrary asset. */
export const DEFAULT_ASSET_STATE = assetsTableSlice.DEFAULT_ASSET_STATE

// ================
// === useStore ===
// ================

export const useStore = zustand.create<assetsTableSlice.AssetsTableSlice>()((...a) => ({
  ...assetsTableSlice.createAssetsTableSlice(...a),
}))
