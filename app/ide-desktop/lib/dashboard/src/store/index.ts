/** @file A bounded store composed of all the slices used in this application. */
import * as React from 'react'

import * as zustand from 'zustand'

import * as assetsTableSlice from '#/store/assetsTableSlice'

// =================
// === Constants ===
// =================

/** The default `AssetState` for an arbitrary asset. */
export const DEFAULT_ASSET_STATE = assetsTableSlice.DEFAULT_ASSET_STATE

// =============
// === Store ===
// =============

/** The type of the global zustand store. */
export interface Store extends assetsTableSlice.AssetsTableSlice {}

// ================
// === useStore ===
// ================

export const useStore = zustand.create<Store>()((...a) => ({
  ...assetsTableSlice.createAssetsTableSlice(...a),
}))

// ===========================
// === useStoreRefInternal ===
// ===========================

/** Return an always-up-to-date ref tracking a value returned by a callback. */
export function useStoreRefInternal<T>(
  extractValue: (state: Store, prevState: Store) => T,
  dependencies: React.DependencyList
) {
  const extractValueRef = React.useRef(extractValue)
  extractValueRef.current = extractValue
  const [initialValue] = React.useState(() => {
    const storeState = useStore.getState()
    return extractValue(storeState, storeState)
  })
  const ref = React.useRef(initialValue)
  React.useEffect(
    () =>
      useStore.subscribe((state, prevState) => {
        ref.current = extractValueRef.current(state, prevState)
      }),
    // eslint-disable-next-line react-hooks/exhaustive-deps
    dependencies
  )
  return ref
}
