/** @file The React provider (and associated hooks) for Data Catalog state. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { EMPTY_SET } from '#/utilities/set'
import { createStore } from '#/utilities/zustand'
import type { DirectoryId } from 'enso-common/src/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'
import { useState, type ReactNode } from 'react'
import { persist } from 'zustand/middleware'
import {
  CurrentDirectoryIdContext,
  DriveContext,
  type CurrentDirectoryIdContextType,
  type DriveStore,
  type ProjectsContextType,
} from './constants'

/** State for {@link categoryIdStore}. */
interface CurrentDirectoryIdStoreState {
  readonly current: CurrentDirectoryIdContextType['currentDirectoryId']
}

const currentDirectoryIdStore = createStore<CurrentDirectoryIdStoreState>()(
  persist((): CurrentDirectoryIdStoreState => ({ current: null }), {
    name: 'enso-current-directory-id',
    version: 2,
  }),
)

/** Props for a {@link DriveProvider}. */
export interface ProjectsProviderProps {
  readonly children:
    | ReactNode
    | ((context: {
        readonly store: ProjectsContextType
        readonly resetAssetTableState: () => void
      }) => ReactNode)
}

/** A React provider for Drive-specific metadata. */
export function DriveProvider(props: ProjectsProviderProps) {
  const { children } = props

  const [currentDirectoryId, privateSetCurrentDirectoryId] = useSearchParamsState<
    CurrentDirectoryIdContextType['currentDirectoryId']
  >('currentDirectoryId', () => currentDirectoryIdStore.getState().current)

  const [store] = useState(() =>
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
    privateSetCurrentDirectoryId(null)
    currentDirectoryIdStore.setState({ current: null })
  })

  const setCurrentDirectoryId = useEventCallback((current: DirectoryId | null) => {
    privateSetCurrentDirectoryId(current)
    currentDirectoryIdStore.setState({ current })
    store.getState().removeSelection()
  })

  return (
    <CurrentDirectoryIdContext.Provider value={{ currentDirectoryId, setCurrentDirectoryId }}>
      <DriveContext.Provider value={store}>
        {typeof children === 'function' ? children({ store, resetAssetTableState }) : children}
      </DriveContext.Provider>
    </CurrentDirectoryIdContext.Provider>
  )
}
