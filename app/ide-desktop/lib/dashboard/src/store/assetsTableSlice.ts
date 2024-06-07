/** @file A store for data used for rendering the assets table. */
import * as defineSlice from '#/store/defineSlice'

import type * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'

// =================
// === Constants ===
// =================

/** The default {@link AssetState} for a {@link backendModule.AnyAsset}. */
const DEFAULT_ASSET_STATE: AssetState = {
  isVisible: true,
  isSelected: false,
  isDragSelected: false,
  isDeleted: false,
  isOpen: false,
  isSelectedForPaste: false,
  temporaryLabels: null,
}

/** The initial {@link AssetState} for each backends. */
const INITIAL_BACKEND_STATE: BackendState = { selectedCount: 0, canDownload: false, assets: {} }

/** The initial {@link AssetState} for all backends. */
const INITIAL_BACKENDS_STATE: Readonly<Record<BackendType, BackendState>> = {
  local: INITIAL_BACKEND_STATE,
  remote: INITIAL_BACKEND_STATE,
}

// ===================
// === BackendType ===
// ===================

/** Possible types for backends. */
export type BackendType = 'local' | 'remote'

// ===========================
// === TemporaryLabelsType ===
// ===========================

/** Variants of temporary label data. */
export type TemporaryLabelsType = 'add' | 'remove'

// =================
// === PasteType ===
// =================

/** Possible types of paste actions. */
export type PasteAction = 'copy' | 'cut'

// =======================
// === TemporaryLabels ===
// =======================

/** Temporary label data */
interface TemporaryLabels {
  readonly type: TemporaryLabelsType
  readonly labels: readonly backendModule.LabelName[]
}

// ==================
// === AssetState ===
// ==================

/** State for a specific {@link backendModule.AnyAsset}. */
export interface AssetState {
  readonly isVisible: boolean
  readonly isSelected: boolean
  readonly isDragSelected: boolean
  // FIXME: Actually set this variable by listening in on mutations
  /** Whether this {@link backendModule.AnyAsset} is in the process of being deleted. */
  readonly isDeleted: boolean
  readonly isOpen: boolean
  readonly isSelectedForPaste: boolean
  readonly temporaryLabels: TemporaryLabels | null
}

// ====================
// === BackendState ===
// ====================

/** State for a specific backend. */
interface BackendState {
  readonly selectedCount: number
  readonly canDownload: boolean
  readonly assets: Readonly<Record<backendModule.AssetId, AssetState>>
}

// ========================
// === AssetsTableSlice ===
// ========================

/** State and actions for this slice. */
export interface AssetsTableSlice {
  readonly backends: Readonly<Record<BackendType, BackendState>>
  readonly assetPasteAction: PasteAction | null
  readonly clearAllAssetsState: () => void
  readonly getAssetState: (backendType: BackendType, assetId: backendModule.AssetId) => AssetState
  readonly setIsAssetOpen: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isOpen: boolean
  ) => void
  readonly toggleIsAssetOpen: (backendType: BackendType, assetId: backendModule.AssetId) => void
  readonly setIsAssetSelected: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isSelected: boolean
  ) => void
  readonly getIsAssetSoleSelected: (
    backendType: BackendType,
    assetId: backendModule.AssetId
  ) => boolean
  readonly setIsAssetDeleted: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isDeleted: boolean
  ) => void
  readonly setAssetsTemporaryLabels: (
    backendType: BackendType,
    assetIds: readonly backendModule.AssetId[],
    temporaryLabels: TemporaryLabels | null
  ) => void
  /** Note: This returns a different object every time, which will affect re-renders. */
  readonly getSelectedAssetIds: (backendType: BackendType) => readonly backendModule.AssetId[]
  readonly setSelectedAssetIds: (
    backendType: BackendType,
    selectedIds: readonly backendModule.AssetId[]
  ) => void
  readonly setDragSelectedAssetIds: (
    backendType: BackendType,
    selectedIds: readonly backendModule.AssetId[]
  ) => void
  readonly setAssetPasteData: (
    backendType: BackendType,
    action: PasteAction,
    ids: readonly backendModule.AssetId[]
  ) => void
  readonly clearAssetPasteData: (backendType: BackendType) => void
}

// ===========================
// === useAssetsTableStore ===
// ===========================

/** A store for data used for rendering the assets table. */
export const createAssetsTableSlice = defineSlice.defineSlice<AssetsTableSlice>()((set, get) => ({
  assetPasteAction: null,
  backends: INITIAL_BACKENDS_STATE,
  clearAllAssetsState: () => {
    set({ backends: INITIAL_BACKENDS_STATE })
  },
  getAssetState: (backendType, assetId) => {
    return get().backends[backendType].assets[assetId] ?? DEFAULT_ASSET_STATE
  },
  setIsAssetOpen: (backendType, assetId, isOpen) => {
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentIsOpen = assetInfo.isOpen
    if (currentIsOpen !== isOpen) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            ...backend,
            assets: { ...assets, ...object.singleKeyObject(assetId, { ...assetInfo, isOpen }) },
          }),
        },
      })
    }
  },
  toggleIsAssetOpen: (backendType, assetId) => {
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
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
    const backend = backends[backendType]
    const assets = backend.assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentIsDeleted = assetInfo.isDeleted
    if (currentIsDeleted !== isDeleted) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            ...backend,
            assets: {
              ...assets,
              ...object.singleKeyObject(assetId, { ...assetInfo, isDeleted }),
            },
          }),
        },
      })
    }
  },
  setIsAssetSelected: (backendType, assetId, isSelected) => {
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    const assetInfo = assets[assetId] ?? DEFAULT_ASSET_STATE
    const currentIsSelected = assetInfo.isSelected
    if (currentIsSelected !== isSelected) {
      set({
        backends: {
          ...backends,
          ...object.singleKeyObject(backendType, {
            ...backend,
            // FIXME: Also update `canDownload`.
            // The new count is guaranteed to be different by exactly 1, because this code only
            // runs if the value of `isSelected` is changed.
            selectedCount: backend.selectedCount + (isSelected ? 1 : -1),
            assets: {
              ...assets,
              ...object.singleKeyObject(assetId, { ...assetInfo, isSelected }),
            },
          }),
        },
      })
    }
  },
  getIsAssetSoleSelected: (backendType, assetId) => {
    const backendState = get().backends[backendType]
    const assetState = backendState.assets[assetId] ?? DEFAULT_ASSET_STATE
    const isSelected = assetState.isSelected
    return isSelected && backendState.selectedCount === 1
  },
  setAssetsTemporaryLabels: (backendType, assetIds, temporaryLabels) => {
    const selectedIdsSet = new Set(assetIds)
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
          selectedCount: assetIds.length,
          assets: {
            ...Object.fromEntries(
              assetIds.map(id => [id, { ...DEFAULT_ASSET_STATE, temporaryLabels }])
            ),
            ...Object.fromEntries(
              object.unsafeEntries(assets).map(kv => {
                const [k, v] = kv
                const effectiveTemporaryLabels = selectedIdsSet.has(k) ? temporaryLabels : null
                return [
                  k,
                  v.temporaryLabels === effectiveTemporaryLabels
                    ? v
                    : { ...v, effectiveTemporaryLabels },
                ]
              })
            ),
          },
        }),
      },
    })
  },
  getSelectedAssetIds: backendType => {
    return object.unsafeEntries(get().backends[backendType].assets).flatMap(kv => {
      const [k, v] = kv
      return v.isSelected ? [k] : []
    })
  },
  setSelectedAssetIds: (backendType, selectedIds) => {
    const selectedIdsSet = new Set(selectedIds)
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
          selectedCount: selectedIds.length,
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
  setDragSelectedAssetIds: (backendType, selectedIds) => {
    const selectedIdsSet = new Set(selectedIds)
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
          selectedCount: selectedIds.length,
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
  setAssetPasteData: (backendType, action, ids) => {
    const idsSet = new Set(ids)
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    set({
      assetPasteAction: action,
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
          assets: {
            ...Object.fromEntries(
              ids.map(id => [id, { ...DEFAULT_ASSET_STATE, isSelectedForPaste: true }])
            ),
            ...Object.fromEntries(
              object.unsafeEntries(assets).map(kv => {
                const [k, v] = kv
                const isSelectedForPaste = idsSet.has(k)
                return [
                  k,
                  v.isSelectedForPaste === isSelectedForPaste ? v : { ...v, isSelectedForPaste },
                ]
              })
            ),
          },
        }),
      },
    })
  },
  clearAssetPasteData: backendType => {
    const backends = get().backends
    const backend = backends[backendType]
    const assets = backend.assets
    set({
      backends: {
        ...backends,
        ...object.singleKeyObject(backendType, {
          ...backend,
          assets: Object.fromEntries(
            object.unsafeEntries(assets).map(kv => {
              const [k, v] = kv
              return [k, !v.isSelectedForPaste ? v : { ...v, isSelectedForPaste: false }]
            })
          ),
        }),
      },
    })
  },
}))
