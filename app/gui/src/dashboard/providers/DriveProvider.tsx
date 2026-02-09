/** @file The React provider (and associated hooks) for Data Catalog state. */
import { useOffline } from '#/hooks/offlineHooks'
import { useSearchParamsState } from '#/hooks/searchParamsStateHooks'
import type { Category, CategoryId } from '#/layouts/CategorySwitcher/Category'
import type { PasteData } from '#/utilities/pasteData'
import { EMPTY_SET } from '#/utilities/set'
import { createStore, resetStoreOnLogout, useStore, type StoreApi } from '#/utilities/zustand'
import { useFullUserSession } from '$/providers/react'
import {
  type AnyAsset,
  type AssetId,
  type BackendType,
  type DirectoryId,
  type LabelName,
} from 'enso-common/src/services/Backend'
import * as React from 'react'
import invariant from 'tiny-invariant'
import { persist } from 'zustand/middleware'
import {
  isCloudCategory,
  useCategories,
  type TransferrableAsset,
} from '../layouts/Drive/Categories'

/** State for {@link driveLocationStore}. */
interface CurrentDirectoryIdStoreState {
  readonly categoryId: CategoryId | null
  readonly directoryId: DirectoryId | null
}

// eslint-disable-next-line react-refresh/only-export-components
export const driveLocationStore = createStore<CurrentDirectoryIdStoreState>()(
  persist((): CurrentDirectoryIdStoreState => ({ categoryId: null, directoryId: null }), {
    name: 'enso-drive-location',
    version: 1,
  }),
)

resetStoreOnLogout(driveLocationStore)

/** Return the full drive location. */
// eslint-disable-next-line react-refresh/only-export-components
export function useCategoryId() {
  return useStore(driveLocationStore, (store) => store.categoryId, { unsafeEnableTransition: true })
}

/** Return the full drive location. */
// eslint-disable-next-line react-refresh/only-export-components
export function getDriveLocation() {
  return driveLocationStore.getState()
}

/** Safely update the drive location. */
// eslint-disable-next-line react-refresh/only-export-components
export function setDriveLocation(directoryId: DirectoryId | null, categoryId?: CategoryId | null) {
  driveLocationStore.setState({
    ...(categoryId !== undefined ? { categoryId } : {}),
    directoryId,
  })
}

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
      Pick<T, keyof T & ('id' | 'labels' | 'parentId' | 'title' | 'type')>
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

/** Data for a context menu. */
export interface ContextMenuData {
  readonly triggerRef: React.MutableRefObject<HTMLElement | null>
  readonly initialContextMenuPosition: Pick<MouseEvent, 'pageX' | 'pageY'> | null
}

/** The state of this zustand store. */
interface DriveStore {
  readonly removeSelection: () => void
  readonly assetToRename: AssetId | null
  readonly setAssetToRename: (assetToRename: AssetId | null) => void
  readonly contextMenuData: ContextMenuData | null
  readonly setContextMenuData: (contextMenuData: ContextMenuData | null) => void
  readonly canDownload: boolean
  readonly setCanDownload: (canDownload: boolean) => void
  readonly pasteData: PasteData<DrivePastePayload> | null
  readonly setPasteData: (pasteData: PasteData<DrivePastePayload> | null) => void
  readonly selectedIds: ReadonlySet<AssetId>
  readonly setSelectedIds: (selectedIds: ReadonlySet<AssetId>) => void
  /** @deprecated Use `selectedIds` instead. */
  readonly selectedAssets: readonly SelectedAssetInfo[]
  readonly setSelectedAssets: (selectedAssets: readonly SelectedAssetInfo[]) => void
  readonly visuallySelectedKeys: ReadonlySet<AssetId> | null
  readonly setVisuallySelectedKeys: (visuallySelectedKeys: ReadonlySet<AssetId> | null) => void
  readonly dragTargetAssetId: AssetId | null
  readonly setDragTargetAssetId: (dragTargetAssetId: AssetId | null) => void
}

/** State contained in a `DriveContext`. */
export type DriveContextType = StoreApi<DriveStore>

const DriveContext = React.createContext<DriveContextType | null>(null)

/** Props for a {@link DriveProvider}. */
export interface DriveProviderProps extends React.PropsWithChildren {}

/** A React provider for Drive-specific metadata. */
export default function DriveProvider(props: DriveProviderProps) {
  const { children } = props

  const { findCategoryById } = useCategories()
  const { user } = useFullUserSession()
  const { isOffline } = useOffline()

  const [currentDirectoryId, privateSetDirectoryId] = useSearchParamsState<DirectoryId | null>(
    'currentDirectoryId',
    () => driveLocationStore.getState().directoryId,
  )

  const [currentCategoryId, privateSetCategoryId, privateResetCategoryId] =
    useSearchParamsState<CategoryId | null>(
      'driveCategory',
      () => {
        const id = getDriveLocation().categoryId
        if (id == null) return null
        const category = findCategoryById(id)
        if (category == null) return null
        const unavailable = (!user.isEnabled || isOffline) && isCloudCategory(category)
        if (unavailable) return null
        return id
      },
      // This is safe, because we confirm the type inside the function.
      // eslint-disable-next-line no-restricted-syntax
      (value): value is CategoryId => findCategoryById(value as CategoryId) != null,
    )

  const [store] = React.useState(() =>
    createStore<DriveStore>((set, get) => ({
      removeSelection: () => {
        set({ selectedIds: new Set(), visuallySelectedKeys: null, selectedAssets: [] })
      },
      assetToRename: null,
      setAssetToRename: (assetToRename) => {
        if (get().assetToRename !== assetToRename) {
          set({ assetToRename })
        }
      },
      contextMenuData: null,
      setContextMenuData: (contextMenuData) => {
        if (get().contextMenuData !== contextMenuData) {
          set({ contextMenuData })
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
      selectedAssets: [],
      setSelectedAssets: (selectedAssets) => {
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
      dragTargetAssetId: null,
      setDragTargetAssetId: (dragTargetAssetId) => {
        if (get().dragTargetAssetId !== dragTargetAssetId) {
          set({ dragTargetAssetId })
        }
      },
    })),
  )

  React.useEffect(() => {
    setDriveLocation(currentDirectoryId, currentCategoryId)
  }, [currentCategoryId, currentDirectoryId])

  React.useEffect(
    () =>
      driveLocationStore.subscribe(({ directoryId, categoryId }, oldState) => {
        if (directoryId !== oldState.directoryId) {
          privateSetDirectoryId(directoryId)
          store.getState().removeSelection()
        }
        if (categoryId !== oldState.categoryId) {
          if (categoryId != null) {
            privateSetCategoryId(categoryId)
          } else {
            privateResetCategoryId()
          }
          store.getState().removeSelection()
        }
      }),
    [privateResetCategoryId, privateSetCategoryId, privateSetDirectoryId, store],
  )

  return <DriveContext.Provider value={store}>{children}</DriveContext.Provider>
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
export function useAssetToRename() {
  const store = useDriveStore()
  return useStore(store, (state) => state.assetToRename)
}

/** A function to set the ID of the most newly created folder. */
// eslint-disable-next-line react-refresh/only-export-components
export function useSetAssetToRename() {
  const store = useDriveStore()
  return useStore(store, (state) => state.setAssetToRename)
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
  return useStore(driveLocationStore, (store) => store.directoryId)
}
