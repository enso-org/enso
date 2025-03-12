/** @file Hooks for `DriveProvider`. */
import { useStore } from '#/hooks/storeHooks'
import type { AssetId } from 'enso-common/src/services/Backend'
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { CurrentDirectoryIdContext, DriveContext } from './constants'

/** The drive store. */
export function useDriveStore() {
  const store = useContext(DriveContext)

  invariant(store, 'Drive store can only be used inside an `DriveProvider`.')

  return store
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

/** The selected keys in the Asset Table. */
export function useSelectedKeys() {
  const store = useDriveStore()
  return useStore(store, (state) => state.selectedIds)
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
  return useStore(store, (state) => state.selectedIds, { unsafeEnableTransition: true })
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

/** The current directory ID. */
export function useCurrentDirectoryId() {
  const context = useContext(CurrentDirectoryIdContext)
  invariant(context, 'Current directory ID can only be used inside an `DriveProvider`.')
  return context.currentDirectoryId
}

/** A function to set the current directory ID. */
export function useSetCurrentDirectoryId() {
  const context = useContext(CurrentDirectoryIdContext)
  invariant(context, 'Current directory ID can only be used inside an `DriveProvider`.')
  return context.setCurrentDirectoryId
}
