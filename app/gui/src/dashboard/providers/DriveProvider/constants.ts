/** @file Constants for `DriveProvider`. */
import type { Category } from '#/layouts/Drive/CategorySwitcher/Category'
import type { TransferrableAsset } from '#/layouts/Drive/CategorySwitcher/useTransferBetweenCategories'
import type { PasteData } from '#/utilities/pasteData'
import type { StoreApi } from '#/utilities/zustand'
import type {
  AnyAsset,
  AssetId,
  BackendType,
  DirectoryId,
  LabelName,
} from 'enso-common/src/services/Backend'
import { createContext } from 'react'

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
export interface DriveStore {
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

export const DriveContext = createContext<ProjectsContextType | null>(null)

/** The current directory ID. */
export interface CurrentDirectoryIdContextType {
  readonly currentDirectoryId: DirectoryId | null
  readonly setCurrentDirectoryId: (nextValue: DirectoryId | null) => void
}

export const CurrentDirectoryIdContext = createContext<CurrentDirectoryIdContextType | null>(null)
