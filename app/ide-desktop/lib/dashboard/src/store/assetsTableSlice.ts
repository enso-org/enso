/** @file A store for data used for rendering the assets table. */
import * as defineSlice from '#/store/defineSlice'

import type * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'
import * as setModule from '#/utilities/set'

// =================
// === Constants ===
// =================

/** The default {@link AssetState} for a {@link backendModule.AnyAsset}. */
const DEFAULT_ASSET_STATE: AssetState = {
  isVisible: true,
  isDeleted: false,
  isOpen: false,
  isSelectedForPaste: false,
}

/** The initial {@link AssetState} for each backends. */
const INITIAL_BACKEND_STATE: BackendState = { canDownload: false, assets: {} }

/** The initial {@link AssetState} for all backends. */
const INITIAL_BACKENDS_STATE: Readonly<Record<BackendType, BackendState>> = {
  local: INITIAL_BACKEND_STATE,
  remote: INITIAL_BACKEND_STATE,
}

// ===================
// === BackendType ===
// ===================

/** Possible types for backends. */
type BackendType = 'local' | 'remote'

// ===========================
// === TemporaryLabelsType ===
// ===========================

/** Variants of temporary label data. */
type TemporaryLabelsType = 'add' | 'remove'

// ==========================
// === TemporaryLabelData ===
// ==========================

/** Temporary label data. */
interface TemporaryLabelData {
  readonly type: TemporaryLabelsType
  readonly labels: readonly backendModule.LabelName[]
  readonly ids: ReadonlySet<backendModule.AssetId>
}

// =================
// === PasteType ===
// =================

/** Possible types of paste actions. */
type PasteAction = 'copy' | 'cut'

// =================
// === PasteData ===
// =================

/** A paste action and the set of keys to be pasted. */
interface PasteData {
  readonly action: PasteAction
  readonly ids: ReadonlySet<backendModule.AssetId>
}

// ==================
// === AssetState ===
// ==================

/** State for a specific {@link backendModule.AnyAsset}. */
export interface AssetState {
  readonly isVisible: boolean
  // FIXME: Actually set this variable by listening in on mutations
  /** Whether this {@link backendModule.AnyAsset} is in the process of being deleted. */
  readonly isDeleted: boolean
  readonly isOpen: boolean
  readonly isSelectedForPaste: boolean
}

// ====================
// === BackendState ===
// ====================

/** State for a specific backend. */
interface BackendState {
  readonly canDownload: boolean
  readonly assets: Readonly<Record<backendModule.AssetId, AssetState>>
}

// ========================
// === AssetsTableSlice ===
// ========================

/** State and actions for this slice. */
export interface AssetsTableSlice {
  readonly backends: Readonly<Record<BackendType, BackendState>>
  // These values are put in global state not to avoid prop drilling, but instead for the
  // fine-grained updates - zustand can subscribe to the results of arbitrary functions, rather
  // than an explicit list of dependencies.
  readonly selectedAssetIds: ReadonlySet<backendModule.AssetId>
  readonly dragSelectedAssetIds: ReadonlySet<backendModule.AssetId>
  readonly assetPasteData: PasteData | null
  readonly temporaryLabelData: TemporaryLabelData | null
  readonly clearAllAssetsState: () => void
  readonly getAssetState: (backendType: BackendType, assetId: backendModule.AssetId) => AssetState
  readonly setIsAssetOpen: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isOpen: boolean
  ) => void
  readonly toggleIsAssetOpen: (backendType: BackendType, assetId: backendModule.AssetId) => void
  readonly getIsAssetSelected: (assetId: backendModule.AssetId) => boolean
  readonly setIsAssetSelected: (assetId: backendModule.AssetId, isSelected: boolean) => void
  readonly getIsAssetSoleSelected: (assetId: backendModule.AssetId) => boolean
  readonly setIsAssetDeleted: (
    backendType: BackendType,
    assetId: backendModule.AssetId,
    isDeleted: boolean
  ) => void
  readonly getSelectedAssetIds: () => ReadonlySet<backendModule.AssetId>
  readonly setSelectedAssetIds: (ids: ReadonlySet<backendModule.AssetId>) => void
  readonly setDragSelectedAssetIds: (ids: ReadonlySet<backendModule.AssetId>) => void
  readonly setAssetPasteData: (data: PasteData | null) => void
  readonly setAssetTemporaryLabelData: (temporaryLabelData: TemporaryLabelData | null) => void
}

// ===========================
// === useAssetsTableStore ===
// ===========================

/** A store for data used for rendering the assets table. */
export const createAssetsTableSlice = defineSlice.defineSlice<AssetsTableSlice>()((set, get) => ({
  backends: INITIAL_BACKENDS_STATE,
  selectedAssetIds: setModule.EMPTY,
  dragSelectedAssetIds: setModule.EMPTY,
  assetPasteData: null,
  temporaryLabelData: null,
  clearAllAssetsState: () => {
    set({
      backends: INITIAL_BACKENDS_STATE,
      selectedAssetIds: setModule.EMPTY,
      assetPasteData: null,
    })
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
  getIsAssetSelected: assetId => {
    return get().selectedAssetIds.has(assetId)
  },
  setIsAssetSelected: (assetId, isSelected) => {
    const ids = get().selectedAssetIds
    const isCurrentlySelected = ids.has(assetId)
    if (isSelected !== isCurrentlySelected) {
      const newIds = new Set(ids)
      if (isSelected) {
        newIds.add(assetId)
        set({ selectedAssetIds: new Set([...ids, assetId]) })
      } else {
        newIds.delete(assetId)
      }
      set({ selectedAssetIds: newIds })
    }
  },
  getIsAssetSoleSelected: assetId => {
    const selectedAssetIds = get().selectedAssetIds
    return selectedAssetIds.size === 1 && selectedAssetIds.has(assetId)
  },
  getSelectedAssetIds: () => {
    return get().selectedAssetIds
  },
  setSelectedAssetIds: ids => {
    set({ selectedAssetIds: ids })
  },
  setDragSelectedAssetIds: ids => {
    set({ dragSelectedAssetIds: ids })
  },
  setAssetPasteData: data => {
    set({ assetPasteData: data })
  },
  setAssetTemporaryLabelData: data => {
    set({ temporaryLabelData: data })
  },
}))
