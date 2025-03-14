/** @file Types for `AssetsTable`. */
import type { Category } from '#/layouts/Drive/Categories/Category'
import type Backend from '#/services/Backend'
import type { AnyAsset, AssetId, DirectoryId } from '#/services/Backend'
import type AssetQuery from '#/utilities/AssetQuery'
import type { SortInfo } from '#/utilities/sorting'
import type { Dispatch, RefObject, SetStateAction } from 'react'
import type { Column, SortableColumn } from './components/columns/types'

/** State passed through from a {@link AssetsTable} to every cell. */
export interface AssetsTableState {
  readonly backend: Backend
  readonly currentDirectoryId: DirectoryId
  readonly scrollContainerRef: RefObject<HTMLElement>
  readonly category: Category
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly setSortInfo: (sortInfo: SortInfo<SortableColumn> | null) => void
  readonly query: AssetQuery
  readonly setQuery: Dispatch<SetStateAction<AssetQuery>>
  readonly hideColumn: (column: Column) => void
  readonly doCopy: () => void
  readonly doCut: () => void
  readonly doPaste: (newParentKey: DirectoryId, newParentId: DirectoryId) => void
  readonly getAssetNodeById: (id: AssetId) => AnyAsset | null
}

/** Data associated with an `AssetRow`, used for rendering. */
export interface AssetRowState {
  readonly isEditingName: boolean
}

/** Common properties for state and setters passed to event handlers on an {@link AssetRow}. */
export interface AssetRowInnerProps {
  readonly asset: AnyAsset
  readonly state: AssetsTableState
  readonly rowState: AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<AssetRowState>>
}
