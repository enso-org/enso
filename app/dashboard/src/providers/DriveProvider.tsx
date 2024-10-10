/** @file The React provider (and associated hooks) for Data Catalog state. */
import * as React from 'react'

import invariant from 'tiny-invariant'
import * as zustand from 'zustand'

import type { AssetPanelContextProps } from '#/layouts/AssetPanel'
import type { Suggestion } from '#/layouts/AssetSearchBar'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import { EMPTY_SET } from '#/utilities/set'
import type { AssetId, DirectoryAsset, DirectoryId } from 'enso-common/src/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'

// ==================
// === DriveStore ===
// ==================

/** The state of this zustand store. */
interface DriveStore {
  readonly category: Category
  readonly setCategory: (category: Category) => void
  readonly targetDirectory: AssetTreeNode<DirectoryAsset> | null
  readonly setTargetDirectory: (targetDirectory: AssetTreeNode<DirectoryAsset> | null) => void
  readonly newestFolderId: DirectoryId | null
  readonly setNewestFolderId: (newestFolderId: DirectoryId | null) => void
  readonly canCreateAssets: boolean
  readonly setCanCreateAssets: (canCreateAssets: boolean) => void
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
  readonly assetPanelProps: AssetPanelContextProps | null
  readonly setAssetPanelProps: (assetPanelProps: AssetPanelContextProps | null) => void
  readonly suggestions: readonly Suggestion[]
  readonly setSuggestions: (suggestions: readonly Suggestion[]) => void
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
    zustand.createStore<DriveStore>((set, get) => ({
      category: { type: 'cloud' },
      setCategory: (category) => {
        if (get().category !== category) {
          set({
            category,
            targetDirectory: null,
            selectedKeys: EMPTY_SET,
            visuallySelectedKeys: null,
          })
        }
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
      selectedKeys: EMPTY_SET,
      setSelectedKeys: (selectedKeys) => {
        if (
          get().selectedKeys !== selectedKeys &&
          (selectedKeys.size !== 0 || get().selectedKeys.size !== 0)
        ) {
          set({ selectedKeys })
        }
      },
      visuallySelectedKeys: null,
      setVisuallySelectedKeys: (visuallySelectedKeys) => {
        if (
          get().visuallySelectedKeys !== visuallySelectedKeys &&
          (visuallySelectedKeys?.size !== 0 || get().visuallySelectedKeys?.size !== 0)
        ) {
          set({ visuallySelectedKeys })
        }
      },
      isAssetPanelPermanentlyVisible: localStorage.get('isAssetPanelVisible') ?? false,
      setIsAssetPanelPermanentlyVisible: (isAssetPanelPermanentlyVisible) => {
        if (get().isAssetPanelPermanentlyVisible !== isAssetPanelPermanentlyVisible) {
          set({ isAssetPanelPermanentlyVisible })
          localStorage.set('isAssetPanelVisible', isAssetPanelPermanentlyVisible)
        }
      },
      isAssetPanelTemporarilyVisible: false,
      setIsAssetPanelTemporarilyVisible: (isAssetPanelTemporarilyVisible) => {
        if (get().isAssetPanelTemporarilyVisible !== isAssetPanelTemporarilyVisible) {
          set({ isAssetPanelTemporarilyVisible })
        }
      },
      assetPanelProps: null,
      setAssetPanelProps: (assetPanelProps) => {
        if (get().assetPanelProps !== assetPanelProps) {
          set({ assetPanelProps })
        }
      },
      suggestions: EMPTY_ARRAY,
      setSuggestions: (suggestions) => {
        if (
          get().suggestions !== suggestions &&
          (suggestions.length !== 0 || get().suggestions.length !== 0)
        ) {
          set({ suggestions })
        }
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

/** Get the category of the Asset Table. */
export function useCategory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.category)
}

/** A function to set the category of the Asset Table. */
export function useSetCategory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setCategory)
}

/** Get the target directory of the Asset Table selection. */
export function useTargetDirectory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.targetDirectory)
}

/** A function to set the target directory of the Asset Table selection. */
export function useSetTargetDirectory() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setTargetDirectory)
}

/** Get the ID of the most newly created folder. */
export function useNewestFolderId() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.newestFolderId)
}

/** A function to set the ID of the most newly created folder. */
export function useSetNewestFolderId() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setNewestFolderId)
}

/** Whether assets can be created in the current directory. */
export function useCanCreateAssets() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.canCreateAssets)
}

/** A function to set whether assets can be created in the current directory. */
export function useSetCanCreateAssets() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setCanCreateAssets)
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

/** Props for the Asset Panel. */
export function useAssetPanelProps() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.assetPanelProps)
}

/** A function to set props for the Asset Panel. */
export function useSetAssetPanelProps() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setAssetPanelProps)
}

/** Search suggestions. */
export function useSuggestions() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.suggestions)
}

/** Set search suggestions. */
export function useSetSuggestions() {
  const store = useDriveStore()
  return zustand.useStore(store, (state) => state.setSuggestions)
}
