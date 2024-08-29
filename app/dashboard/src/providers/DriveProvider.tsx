/** @file The React provider (and associated hooks) for Data Catalog state. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as zustand from 'zustand'

import { useLocalStorage } from '#/providers/LocalStorageProvider'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type { AssetId, DirectoryAsset } from 'enso-common/src/services/Backend'

// ==================
// === DriveStore ===
// ==================

/** The state of this zustand store. */
interface DriveStore {
  readonly targetDirectory: AssetTreeNode<DirectoryAsset> | null
  readonly setTargetDirectory: (targetDirectory: AssetTreeNode<DirectoryAsset> | null) => void
  readonly canDownload: boolean
  readonly setCanDownload: (canDownload: boolean) => void
  readonly selectedKeys: ReadonlySet<AssetId>
  readonly setSelectedKeys: (selectedKeys: ReadonlySet<AssetId>) => void
  readonly visuallySelectedKeys: ReadonlySet<AssetId> | null
  readonly setVisuallySelectedKeys: (visuallySelectedKeys: ReadonlySet<AssetId> | null) => void
  readonly isAssetPanelPermanentlyVisible: boolean
  readonly setIsAssetPanelPermanentlyVisible: (isAssetPanelTemporarilyVisible: boolean) => void
  readonly isAssetPanelTemporarilyVisible: boolean
  readonly setIsAssetPanelTemporarilyVisible: (isAssetPanelTemporarilyVisible: boolean) => void
}

// =======================
// === ProjectsContext ===
// =======================

/** State contained in a `ProjectsContext`. */
export interface ProjectsContextType extends zustand.StoreApi<DriveStore> {}

const DriveContext = React.createContext<ProjectsContextType | null>(null)

/** Props for a {@link DriveProvider}. */
export interface ProjectsProviderProps extends Readonly<React.PropsWithChildren> {}

// ========================
// === ProjectsProvider ===
// ========================

/** A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused. */
export default function DriveProvider(props: ProjectsProviderProps) {
  const { children } = props
  const { localStorage } = useLocalStorage()
  const [store] = React.useState(() =>
    zustand.createStore<DriveStore>((set) => ({
      targetDirectory: null,
      setTargetDirectory: (targetDirectory) => {
        set({ targetDirectory })
      },
      canDownload: false,
      setCanDownload: (canDownload) => {
        set({ canDownload })
      },
      selectedKeys: new Set(),
      setSelectedKeys: (selectedKeys) => {
        set({ selectedKeys })
      },
      visuallySelectedKeys: null,
      setVisuallySelectedKeys: (visuallySelectedKeys) => {
        set({ visuallySelectedKeys })
      },
      isAssetPanelPermanentlyVisible: localStorage.get('isAssetPanelVisible') ?? false,
      setIsAssetPanelPermanentlyVisible: (isAssetPanelPermanentlyVisible) => {
        set({ isAssetPanelPermanentlyVisible })
        localStorage.set('isAssetPanelVisible', isAssetPanelPermanentlyVisible)
      },
      isAssetPanelTemporarilyVisible: false,
      setIsAssetPanelTemporarilyVisible: (isAssetPanelTemporarilyVisible) => {
        set({ isAssetPanelTemporarilyVisible })
      },
    })),
  )

  return <DriveContext.Provider value={store}>{children}</DriveContext.Provider>
}

/** The drive store. */
export function useDriveStore() {
  const store = React.useContext(DriveContext)

  invariant(store, 'Drive store can only be used inside an `DriveProvider`.')

  return store
}

/** A function to get the target directory of the Asset Table selection. */
export function useTargetDirectory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.targetDirectory)
}

/** A function to set the target directory of the Asset Table selection. */
export function useSetTargetDirectory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setTargetDirectory)
}

/** Whether the current Asset Table selection is downloadble. */
export function useCanDownload() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.canDownload)
}

/** A function to set whether the current Asset Table selection is downloadble. */
export function useSetCanDownload() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setCanDownload)
}

/** The selected keys in the Asset Table. */
export function useSelectedKeys() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.selectedKeys)
}

/** A function to set the selected keys of the Asset Table selection. */
export function useSetSelectedKeys() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setSelectedKeys)
}

/** The visually selected keys in the Asset Table. */
export function useVisuallySelectedKeys() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.selectedKeys)
}

/** A function to set the visually selected keys in the Asset Table. */
export function useSetVisuallySelectedKeys() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setVisuallySelectedKeys)
}

/** Whether the Asset Panel is toggled on. */
export function useIsAssetPanelPermanentlyVisible() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.isAssetPanelPermanentlyVisible)
}

/** A function to set whether the Asset Panel is toggled on. */
export function useSetIsAssetPanelPermanentlyVisible() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setIsAssetPanelPermanentlyVisible)
}

/** Whether the Asset Panel is currently visible (e.g. for editing a Datalink). */
export function useIsAssetPanelTemporarilyVisible() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.isAssetPanelTemporarilyVisible)
}

/** A function to set whether the Asset Panel is currently visible (e.g. for editing a Datalink). */
export function useSetIsAssetPanelTemporarilyVisible() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setIsAssetPanelTemporarilyVisible)
}

/** Whether the Asset Panel is currently visible, either temporarily or permanently. */
export function useIsAssetPanelVisible() {
  const isAssetPanelPermanentlyVisible = useIsAssetPanelPermanentlyVisible()
  const isAssetPanelTemporarilyVisible = useIsAssetPanelTemporarilyVisible()
  return isAssetPanelPermanentlyVisible || isAssetPanelTemporarilyVisible
}
