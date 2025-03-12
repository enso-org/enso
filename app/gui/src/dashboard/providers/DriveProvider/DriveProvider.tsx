/** @file The React provider (and associated hooks) for Data Catalog state. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import { EMPTY_SET } from '#/utilities/set'
import { createStore } from '#/utilities/zustand'
import type { DirectoryId } from 'enso-common/src/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'
import { useState, type ReactNode } from 'react'
import {
  CurrentDirectoryIdContext,
  DriveContext,
  type CurrentDirectoryIdContextType,
  type DriveStore,
  type ProjectsContextType,
} from './constants'

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
  >('currentDirectoryId', { current: null, parent: null })

  const [store] = useState(() =>
    createStore<DriveStore>((set, get) => ({
      removeSelection: () => {
        set({ selectedIds: EMPTY_SET, visuallySelectedKeys: null })
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
  })

  const setCurrentDirectoryId = useEventCallback(
    ({ current, parent }: { current: DirectoryId | null; parent: DirectoryId | null }) => {
      privateSetCurrentDirectoryId({ current, parent })
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
