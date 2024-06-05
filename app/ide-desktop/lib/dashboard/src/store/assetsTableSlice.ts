/** @file A store for data used for rendering the assets table. */
import * as defineSlice from '#/store/defineSlice'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** The default {@link AssetState} for a {@link backendModule.AnyAsset}. */
export const DEFAULT_ASSET_STATE: AssetState = {
  isVisible: true,
  isSelected: false,
  isDeleted: false,
  isOpen: false,
  children: null,
}

// ===================
// === BackendType ===
// ===================

/** Possible types for backends. */
export type BackendType = 'local' | 'remote'

// ==================
// === AssetState ===
// ==================

/** State for a specific {@link backendModule.AnyAsset}. */
export interface AssetState {
  readonly isVisible: boolean
  readonly isSelected: boolean
  /** Whether this {@link backendModule.AnyAsset} is in the process of being deleted. */
  readonly isDeleted: boolean
  readonly isOpen: boolean
  // FIXME: why is `children` needed?
  readonly children: readonly backendModule.AnyAsset[] | null
}

// ====================
// === BackendState ===
// ====================

/** State for a specific backend. */
interface BackendState {
  readonly assets: Record<backendModule.AssetId, AssetState>
}

// ========================
// === AssetsTableSlice ===
// ========================

/** State and actions for this slice. */
export interface AssetsTableSlice {
  readonly backends: Record<BackendType, BackendState>
  readonly setIsAssetOpen: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isOpen: boolean
  ) => void
  readonly toggleIsAssetOpen: (backendType: BackendType, assetId: backendModule.AssetId) => void
  readonly setIsAssetDeleted: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isDeleted: boolean
  ) => void
  readonly setAssetChildren: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    chilren: readonly backendModule.AnyAsset[]
  ) => void
  /** Note: This returns a different object every time, which will affect re-renders. */
  readonly getSelectedIds: (backendType: BackendType) => readonly backendModule.AssetId[]
  readonly setSelectedIds: (
    backendType: BackendType,
    selectedIds: readonly backendModule.AssetId[]
  ) => void
  readonly getTargetDirectoryId: (
    backend: Backend,
    rootDirectoryId: backendModule.DirectoryId
  ) => backendModule.DirectoryId
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
  getSelectedIds: backendType => {
    return object.unsafeEntries(get().backends[backendType].assets).flatMap(kv => {
      const [k, v] = kv
      return v.isSelected ? [k] : []
    })
  },
  setSelectedIds: (backendType, selectedIds) => {
    const selectedIdsSet = new Set(selectedIds)
    const backends = get().backends
    const assets = backends[backendType].assets
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          assets: {
            ...Object.fromEntries(
              selectedIds.map(id => [id, { ...DEFAULT_ASSET_STATE, isSelected: true }])
            ),
            ...Object.fromEntries(
              object.unsafeEntries(assets).map(kv => {
                const [k, v] = kv
                const isSelected = selectedIdsSet.has(k)
                return [k, v.isSelected === isSelected ? v : { ...v, isSelected }]
              })
            ),
          },
        }),
      },
    })
  },
  getTargetDirectoryId: (backend, rootDirectoryId) => {
    const assets = get().backends[backend.type].assets
    const selectedAssetsEntries = object.unsafeEntries(assets).filter(kv => {
      const [, v] = kv
      return v.isSelected
    })
    let commonDirectoryId: backendModule.DirectoryId | null = null
    let otherCandidateDirectoryId: backendModule.DirectoryId | null = null
    for (const key of selectedAssetsEntries) {
      const asset = nodeMapRef.current.get(key)
      if (asset != null) {
        if (commonDirectoryId == null) {
          commonDirectoryId = asset.parentId
          otherCandidateDirectoryId =
            asset.type === backendModule.AssetType.directory ? asset.id : null
        } else if (asset.id === commonDirectoryId || asset.parentId === commonDirectoryId) {
          otherCandidateDirectoryId = null
        } else if (
          otherCandidateDirectoryId != null &&
          (asset.id === otherCandidateDirectoryId || asset.parentId === otherCandidateDirectoryId)
        ) {
          commonDirectoryId = otherCandidateDirectoryId
          otherCandidateDirectoryId = null
        } else {
          // No match; there is no common parent directory for the entire selection.
          commonDirectoryId = null
          break
        }
      }
    }
    return commonDirectoryId ?? rootDirectoryId
  },
}))
