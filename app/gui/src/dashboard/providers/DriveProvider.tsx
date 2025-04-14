/** @file The React provider (and associated hooks) for Data Catalog state. */
import * as React from 'react'

import { createStore, useStore, type StoreApi } from '#/utilities/zustand'
import invariant from 'tiny-invariant'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import type { PasteData } from '#/utilities/pasteData'
import { EMPTY_SET } from '#/utilities/set'
import {
  type AnyAsset,
  type AssetId,
  type BackendType,
  type DirectoryId,
  type LabelName,
} from 'enso-common/src/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'
import { persist } from 'zustand/middleware'
import type { TransferrableAsset } from '../layouts/Drive/Categories'

/** State for {@link categoryIdStore}. */
type CurrentDirectoryIdStoreState = CurrentDirectoryIdContextType['currentDirectoryId']

const currentDirectoryIdStore = createStore<CurrentDirectoryIdStoreState>()(
  persist(
    (): CurrentDirectoryIdStoreState => ({
      current: null,
      parent: null,
    }),
    { name: 'enso-current-directory-id', version: 1 },
  ),
)

/** Attached data for a paste payload. */
export interface DrivePastePayload {
  readonly backendType: BackendType
  readonly category: Category
  readonly assets: readonly TransferrableAsset[]
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

/** A single directory in the breadcrumbs. */
export interface DirectoryPath {
  readonly id: DirectoryId
  readonly name: string
}

/** The state of this zustand store. */
interface DriveStore {
  readonly removeSelection: () => void
  readonly newestFolderId: DirectoryId | null
  readonly setNewestFolderId: (newestFolderId: DirectoryId | null) => void
  readonly canDownload: boolean
  readonly setCanDownload: (canDownload: boolean) => void
  readonly pasteData: PasteData<DrivePastePayload> | null
  readonly setPasteData: (pasteData: PasteData<DrivePastePayload> | null) => void
  readonly selectedIds: ReadonlySet<AssetId>
  readonly setSelectedIds: (selectedIds: ReadonlySet<AssetId>) => void
  /**
   * @deprecated Use `selectedIds` instead.
   */
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
}

/** State contained in a `ProjectsContext`. */
export type ProjectsContextType = StoreApi<DriveStore>

const DriveContext = React.createContext<ProjectsContextType | null>(null)

/** The current directory ID. */
interface CurrentDirectoryIdContextType {
  readonly currentDirectoryId: {
    readonly current: DirectoryId | null
    readonly parent: DirectoryId | null
  }
  readonly setCurrentDirectoryId: (nextValue: {
    readonly current: DirectoryId | null
    readonly parent: DirectoryId | null
  }) => void
}

const CurrentDirectoryIdContext = React.createContext<CurrentDirectoryIdContextType | null>(null)

/** Props for a {@link DriveProvider}. */
export interface ProjectsProviderProps {
  readonly children:
    | React.ReactNode
    | ((context: {
        readonly store: ProjectsContextType
        readonly resetAssetTableState: () => void
      }) => React.ReactNode)
}

/** A React provider for Drive-specific metadata. */
export default function DriveProvider(props: ProjectsProviderProps) {
  const { children } = props

  const [currentDirectoryId, privateSetCurrentDirectoryId] = useSearchParamsState<
    CurrentDirectoryIdContextType['currentDirectoryId']
  >('currentDirectoryId', () => currentDirectoryIdStore.getState())

  const [store] = React.useState(() =>
    createStore<DriveStore>((set, get) => ({
      removeSelection: () => {
        set({ selectedIds: new Set(), visuallySelectedKeys: null, selectedAssets: [] })
      },
      newestFolderId: null,
      setNewestFolderId: (newestFolderId) => {
        if (get().newestFolderId !== newestFolderId) {
          set({ newestFolderId })
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
      selectedIds: EMPTY_SET,
      setSelectedIds: (selectedIds) => {
        set({ selectedIds })
      },
      selectedAssets: EMPTY_ARRAY,
      setSelectedAssets: (selectedAssets) => {
        if (selectedAssets.length === 0) {
          selectedAssets = EMPTY_ARRAY
        }
        if (get().selectedAssets !== selectedAssets) {
          set({
            selectedAssets,
            selectedIds:
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
    })),
  )

  const resetAssetTableState = useEventCallback(() => {
    store.getState().removeSelection()
    privateSetCurrentDirectoryId({ current: null, parent: null })
    currentDirectoryIdStore.setState({ current: null, parent: null })
  })

  const setCurrentDirectoryId = useEventCallback(
    ({ current, parent }: { current: DirectoryId | null; parent: DirectoryId | null }) => {
      privateSetCurrentDirectoryId({ current, parent })
      currentDirectoryIdStore.setState({ current, parent })
      store.getState().removeSelection()
    },
  )

  return (
    <CurrentDirectoryIdContext.Provider value={{ currentDirectoryId, setCurrentDirectoryId }}>
      <DriveContext.Provider value={store}>
        {typeof children === 'function' ? children({ store, resetAssetTableState }) : children}
      </DriveContext.Provider>
    </CurrentDirectoryIdContext.Provider>
  )
}

/** The drive store. */
// eslint-disable-next-line react-refresh/only-export-components
export function useDriveStore() {
  const store = React.useContext(DriveContext)

  invariant(store, 'Drive store can only be used inside an `DriveProvider`.')

  return store
}

/** The ID of the most newly created folder. */
// eslint-disable-next-line react-refresh/only-export-components
export function useNewestFolderId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.newestFolderId)
}

/** A function to set the ID of the most newly created folder. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetNewestFolderId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setNewestFolderId)
}

/** Whether the current Asset Table selection is downloadble. */
// eslint-disable-next-line react-refresh/only-export-components
export function useCanDownload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.canDownload)
}

/** A function to set whether the current Asset Table selection is downloadble. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetCanDownload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setCanDownload)
}

/** The paste data for the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function usePasteData() {
  const store = useDriveStore()
  return useStore(store, (state) => state.pasteData)
}

/** A function to set the paste data for the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetPasteData() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setPasteData)
}

/** The selected keys in the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSelectedIds() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedIds)
}

/** A function to set the selected keys in the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetSelectedIds() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setSelectedIds)
}

/**
 * The selected assets in the Asset Table.
 * @deprecated Use `useSelectedIds` instead.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useSelectedAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedAssets)
}

/**
 * A function to set the selected assets in the Asset Table.
 * @deprecated Use `useSetSelectedIds` instead.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetSelectedAssets() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setSelectedAssets)
}

/** The visually selected keys in the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function useVisuallySelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedIds, { unsafeEnableTransition: true })
}

/** A function to set the visually selected keys in the Asset Table. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetVisuallySelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setVisuallySelectedKeys, { unsafeEnableTransition: true })
}

/** The drag payload of labels. */
// eslint-disable-next-line react-refresh/only-export-components
export function useLabelsDragPayload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.labelsDragPayload)
}

/** A function to set the drag payload of labels. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetLabelsDragPayload() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setLabelsDragPayload)
}

/**
 * Whether dragging is currently active for a selected row.
 * This is true if and only if this row, or another selected row, is being dragged over.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useIsDraggingOverSelectedRow(selected: boolean) {
  const store = useDriveStore()
  return useStore(store, (state) => selected && state.isDraggingOverSelectedRow)
}

/** A function to set whether dragging is currently over a selected row. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetIsDraggingOverSelectedRow() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setIsDraggingOverSelectedRow)
}

/** Whether the given {@link AssetId} is the one currently being dragged over. */
// eslint-disable-next-line react-refresh/only-export-components
export function useIsDragTargetAssetId(assetId: AssetId) {
  const store = useDriveStore()
  return useStore(store, (state) => assetId === state.dragTargetAssetId)
}

/** A function to set which {@link AssetId} is the one currently being dragged over. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetDragTargetAssetId() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setDragTargetAssetId)
}

/** The current directory ID. */
// eslint-disable-next-line react-refresh/only-export-components
export function useCurrentDirectoryId() {
  const context = React.useContext(CurrentDirectoryIdContext)

  invariant(context, 'Current directory ID can only be used inside an `DriveProvider`.')

  return context.currentDirectoryId
}

/** A function to set the current directory ID. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetCurrentDirectoryId() {
  const context = React.useContext(CurrentDirectoryIdContext)

  invariant(context, 'Current directory ID can only be used inside an `DriveProvider`.')

  return context.setCurrentDirectoryId
}
