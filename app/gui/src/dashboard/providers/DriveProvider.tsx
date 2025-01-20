/** @file The React provider (and associated hooks) for Data Catalog state. */
import * as React from 'react'

import { createStore, useStore, type StoreApi } from '#/utilities/zustand'
import invariant from 'tiny-invariant'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type { AnyAssetTreeNode } from '#/utilities/AssetTreeNode'
import type { PasteData } from '#/utilities/pasteData'
import { EMPTY_SET } from '#/utilities/set'
import type {
  AnyAsset,
  AssetId,
  BackendType,
  DirectoryAsset,
  DirectoryId,
  LabelName,
} from 'enso-common/src/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'
import { unsafeMutable } from 'enso-common/src/utilities/data/object'

// ==================
// === DriveStore ===
// ==================

/** Attached data for a paste payload. */
export interface DrivePastePayload {
  readonly backendType: BackendType
  readonly category: Category
  readonly ids: ReadonlySet<AssetId>
}

/** The subset of asset information required for selections. */
export type SelectedAssetInfo =
  AnyAsset extends infer T ?
    T extends T ?
      Pick<T, keyof T & ('id' | 'parentId' | 'title' | 'type')>
    : never
  : never

/** Payload for labels being dragged. */
export interface LabelsDragPayload {
  readonly typeWhenAppliedToSelection: 'add' | 'remove'
  readonly labels: readonly LabelName[]
}

/** The state of this zustand store. */
interface DriveStore {
  readonly resetAssetTableState: () => void
  readonly targetDirectory: AssetTreeNode<DirectoryAsset> | null
  readonly setTargetDirectory: (targetDirectory: AssetTreeNode<DirectoryAsset> | null) => void
  readonly newestFolderId: DirectoryId | null
  readonly setNewestFolderId: (newestFolderId: DirectoryId | null) => void
  readonly canCreateAssets: boolean
  readonly setCanCreateAssets: (canCreateAssets: boolean) => void
  readonly canDownload: boolean
  readonly setCanDownload: (canDownload: boolean) => void
  readonly pasteData: PasteData<DrivePastePayload> | null
  readonly setPasteData: (pasteData: PasteData<DrivePastePayload> | null) => void
  readonly expandedDirectoryIds: readonly DirectoryId[]
  readonly setExpandedDirectoryIds: (selectedKeys: readonly DirectoryId[]) => void
  readonly selectedKeys: ReadonlySet<AssetId>
  readonly selectedAssets: readonly SelectedAssetInfo[]
  readonly setSelectedAssets: (selectedAssets: readonly SelectedAssetInfo[]) => void
  readonly visuallySelectedKeys: ReadonlySet<AssetId> | null
  readonly setVisuallySelectedKeys: (visuallySelectedKeys: ReadonlySet<AssetId> | null) => void
  readonly labelsDragPayload: LabelsDragPayload | null
  readonly setLabelsDragPayload: (labelsDragPayload: LabelsDragPayload | null) => void
  readonly isDraggingOverSelectedRow: boolean
  readonly setIsDraggingOverSelectedRow: (isDraggingOverSelectedRow: boolean) => void
  readonly dragTargetAssetId: AssetId | null
  readonly setDragTargetAssetId: (dragTargetAssetId: AssetId | null) => void
  readonly nodeMap: { readonly current: ReadonlyMap<AssetId, AnyAssetTreeNode> }
  readonly setNodeMap: (nodeMap: ReadonlyMap<AssetId, AnyAssetTreeNode>) => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export type ProjectsContextType = StoreApi<DriveStore>

const DriveContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link DriveProvider}. */
export interface ProjectsProviderProps {
  readonly children:
    | React.ReactNode
    | ((context: {
        readonly store: ProjectsContextType
        readonly resetAssetTableState: () => void
      }) => React.ReactNode)
}

// ========================
// === ProjectsProvider ===
// ========================

/** A React provider for Drive-specific metadata. */
export default function DriveProvider(props: ProjectsProviderProps) {
  const { children } = props

  const [store] = React.useState(() =>
    createStore<DriveStore>((set, get) => ({
      resetAssetTableState: () => {
        set({
          targetDirectory: null,
          selectedKeys: EMPTY_SET,
          visuallySelectedKeys: null,
          expandedDirectoryIds: EMPTY_ARRAY,
        })
      },
      targetDirectory: null,
      setTargetDirectory: (targetDirectory) => {
        if (get().targetDirectory !== targetDirectory) {
          set({ targetDirectory })
        }
      },
      newestFolderId: null,
      setNewestFolderId: (newestFolderId) => {
        if (get().newestFolderId !== newestFolderId) {
          set({ newestFolderId })
        }
      },
      canCreateAssets: true,
      setCanCreateAssets: (canCreateAssets) => {
        if (get().canCreateAssets !== canCreateAssets) {
          set({ canCreateAssets })
        }
      },
      canDownload: false,
      setCanDownload: (canDownload) => {
        if (get().canDownload !== canDownload) {
          set({ canDownload })
        }
      },
      pasteData: null,
      setPasteData: (pasteData) => {
        if (get().pasteData !== pasteData) {
          set({ pasteData })
        }
      },
      expandedDirectoryIds: EMPTY_ARRAY,
      setExpandedDirectoryIds: (expandedDirectoryIds) => {
        if (get().expandedDirectoryIds !== expandedDirectoryIds) {
          set({ expandedDirectoryIds })
        }
      },
      selectedKeys: EMPTY_SET,
      selectedAssets: EMPTY_ARRAY,
      setSelectedAssets: (selectedAssets) => {
        if (selectedAssets.length === 0) {
          selectedAssets = EMPTY_ARRAY
        }
        if (get().selectedAssets !== selectedAssets) {
          set({
            selectedAssets,
            selectedKeys:
              selectedAssets.length === 0 ?
                EMPTY_SET
              : new Set(selectedAssets.map((asset) => asset.id)),
          })
        }
      },
      visuallySelectedKeys: null,
      setVisuallySelectedKeys: (visuallySelectedKeys) => {
        set({ visuallySelectedKeys })
      },
      labelsDragPayload: null,
      setLabelsDragPayload: (labelsDragPayload) => {
        if (get().labelsDragPayload !== labelsDragPayload) {
          set({ labelsDragPayload })
        }
      },
      isDraggingOverSelectedRow: false,
      setIsDraggingOverSelectedRow: (isDraggingOverSelectedRow) => {
        if (get().isDraggingOverSelectedRow !== isDraggingOverSelectedRow) {
          set({ isDraggingOverSelectedRow })
        }
      },
      dragTargetAssetId: null,
      setDragTargetAssetId: (dragTargetAssetId) => {
        if (get().dragTargetAssetId !== dragTargetAssetId) {
          set({ dragTargetAssetId })
        }
      },
      nodeMap: { current: new Map() },
      setNodeMap: (nodeMap) => {
        if (get().nodeMap.current !== nodeMap) {
          unsafeMutable(get().nodeMap).current = nodeMap
          set({ nodeMap: get().nodeMap })
        }
      },
    })),
  )

  const resetAssetTableState = useStore(store, (state) => state.resetAssetTableState)

  return (
    <DriveContext.Provider value={store}>
      {typeof children === 'function' ? children({ store, resetAssetTableState }) : children}
    </DriveContext.Provider>
  )
}

/** The drive store. */
export function useDriveStore() {
  const store = React.useContext(DriveContext)

  invariant(store, 'Drive store can only be used inside an `DriveProvider`.')

  return store
}

/** The target directory of the Asset Table selection. */
export function useTargetDirectory() {
  const store = useDriveStore()
  return useStore(store, (state) => state.targetDirectory)
}

/** A function to set the target directory of the Asset Table selection. */
export function useSetTargetDirectory() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setTargetDirectory)
}

/** The ID of the most newly created folder. */
export function useNewestFolderId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.newestFolderId)
}

/** A function to set the ID of the most newly created folder. */
export function useSetNewestFolderId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setNewestFolderId)
}

/** Whether assets can be created in the current directory. */
export function useCanCreateAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.canCreateAssets)
}

/** A function to set whether assets can be created in the current directory. */
export function useSetCanCreateAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setCanCreateAssets)
}

/** Whether the current Asset Table selection is downloadble. */
export function useCanDownload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.canDownload)
}

/** A function to set whether the current Asset Table selection is downloadble. */
export function useSetCanDownload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setCanDownload)
}

/** The paste data for the Asset Table. */
export function usePasteData() {
  const store = useDriveStore()
  return useStore(store, (state) => state.pasteData)
}

/** A function to set the paste data for the Asset Table. */
export function useSetPasteData() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setPasteData)
}

/** The expanded directories in the Asset Table. */
export function useExpandedDirectoryIds() {
  const store = useDriveStore()
  return useStore(store, (state) => state.expandedDirectoryIds)
}

/** A function to set the expanded directoyIds in the Asset Table. */
export function useSetExpandedDirectoryIds() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setExpandedDirectoryIds, {
    unsafeEnableTransition: true,
  })
}

/** The selected keys in the Asset Table. */
export function useSelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedKeys)
}

/** The selected assets in the Asset Table. */
export function useSelectedAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedAssets)
}

/** A function to set the selected assets in the Asset Table. */
export function useSetSelectedAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setSelectedAssets)
}

/** The visually selected keys in the Asset Table. */
export function useVisuallySelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedKeys, { unsafeEnableTransition: true })
}

/** A function to set the visually selected keys in the Asset Table. */
export function useSetVisuallySelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setVisuallySelectedKeys, { unsafeEnableTransition: true })
}

/** The drag payload of labels. */
export function useLabelsDragPayload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.labelsDragPayload)
}

/** A function to set the drag payload of labels. */
export function useSetLabelsDragPayload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setLabelsDragPayload)
}

/** The map of keys to {@link AssetTreeNode}s. */
export function useNodeMap() {
  const store = useDriveStore()
  return useStore(store, (state) => state.nodeMap)
}

/** A function to set the map of keys to {@link AssetTreeNode}s. */
export function useSetNodeMap() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setNodeMap)
}

/**
 * Whether dragging is currently active for a selected row.
 * This is true if and only if this row, or another selected row, is being dragged over.
 */
export function useIsDraggingOverSelectedRow(selected: boolean) {
  const store = useDriveStore()
  return useStore(store, (state) => selected && state.isDraggingOverSelectedRow)
}

/** A function to set whether dragging is currently over a selected row. */
export function useSetIsDraggingOverSelectedRow() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setIsDraggingOverSelectedRow)
}

/** Whether the given {@link AssetId} is the one currently being dragged over. */
export function useIsDragTargetAssetId(assetId: AssetId) {
  const store = useDriveStore()
  return useStore(store, (state) => assetId === state.dragTargetAssetId)
}

/** A function to set which {@link AssetId} is the one currently being dragged over. */
export function useSetDragTargetAssetId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setDragTargetAssetId)
}

/** Toggle whether a specific directory is expanded. */
export function useToggleDirectoryExpansion() {
  const driveStore = useDriveStore()
  const setExpandedDirectoryIds = useSetExpandedDirectoryIds()

  return useEventCallback((directoryId: DirectoryId, override?: boolean) => {
    const expandedDirectoryIds = driveStore.getState().expandedDirectoryIds
    const isExpanded = expandedDirectoryIds.includes(directoryId)
    const shouldExpand = override ?? !isExpanded

    if (shouldExpand !== isExpanded) {
      React.startTransition(() => {
        if (shouldExpand) {
          setExpandedDirectoryIds([...expandedDirectoryIds, directoryId])
        } else {
          setExpandedDirectoryIds(expandedDirectoryIds.filter((id) => id !== directoryId))
        }
      })
    }
  })
}
