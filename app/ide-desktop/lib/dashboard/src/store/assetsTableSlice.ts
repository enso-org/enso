/** @file A store for data used for rendering the assets table. */
import * as defineSlice from '#/store/defineSlice'

import type * as backend from '#/services/Backend'

import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** The default {@link AssetState} for a {@link backend.AnyAsset}. */
export const DEFAULT_ASSET_STATE: AssetState = {
  isVisible: true,
  isDeleted: false,
  isOpen: false,
  children: [],
}

// ===================
// === BackendType ===
// ===================

/** Possible types for backends. */
export type BackendType = 'local' | 'remote'

// ==================
// === AssetState ===
// ==================

/** State for a specific {@link backend.AnyAsset}. */
export interface AssetState {
  readonly isVisible: boolean
  /** Whether this {@link backend.AnyAsset} is in the process of being deleted. */
  readonly isDeleted: boolean
  readonly isOpen: boolean
  readonly children: readonly backend.AnyAsset[]
}

// ====================
// === BackendState ===
// ====================

/** State for a specific backend. */
interface BackendState {
  readonly assets: Record<backend.AssetId, AssetState>
}

// ========================
// === AssetsTableSlice ===
// ========================

/** State and actions for this slice. */
export interface AssetsTableSlice {
  readonly backends: Record<BackendType, BackendState>
  readonly setIsAssetOpen: (
    backendType: BackendType,
    assetId: backend.AssetId,
    isOpen: boolean
  ) => void
  readonly toggleIsAssetOpen: (backendType: BackendType, assetId: backend.AssetId) => void
  readonly setIsAssetDeleted: (
    backendType: BackendType,
    assetId: backend.AssetId,
    isDeleted: boolean
  ) => void
  readonly setAssetChildren: (
    backendType: BackendType,
    assetId: backend.AssetId,
    chilren: readonly backend.AnyAsset[]
  ) => void
}

// ===========================
// === useAssetsTableStore ===
// ===========================

/** A store for data used for rendering the assets table. */
export const createAssetsTableSlice = defineSlice.defineSlice<AssetsTableSlice>()((set, get) => ({
  backends: { local: { assets: {} }, remote: { assets: {} } },
  setIsAssetOpen: (backendType, assetId, isOpen) => {
    const backends = get().backends
    const assets = backends[backendType].assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentIsOpen = assetInfo.isOpen
    if (currentIsOpen !== isOpen) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            assets: { ...assets, ...object.singleKeyObject(assetId, { ...assetInfo, isOpen }) },
          }),
        },
      })
    }
  },
  toggleIsAssetOpen: (backendType, assetId) => {
    const backends = get().backends
    const assets = backends[backendType].assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          assets: {
            ...assets,
            ...object.singleKeyObject(assetId, { ...assetInfo, isOpen: !assetInfo.isOpen }),
          },
        }),
      },
    })
  },
  setIsAssetDeleted: (backendType, assetId, isDeleted) => {
    const backends = get().backends
    const assets = backends[backendType].assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentIsDeleted = assetInfo.isDeleted
    if (currentIsDeleted !== isDeleted) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            assets: { ...assets, ...object.singleKeyObject(assetId, { ...assetInfo, isDeleted }) },
          }),
        },
      })
    }
  },
  setAssetChildren: (backendType, assetId, children) => {
    const backends = get().backends
    const assets = backends[backendType].assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentChildren = assetInfo.children
    if (currentChildren !== children) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            assets: { ...assets, ...object.singleKeyObject(assetId, { ...assetInfo, children }) },
          }),
        },
      })
    }
  },
}))
