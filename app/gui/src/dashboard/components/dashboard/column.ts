/** @file Column types and column display modes. */
import { memo, type Dispatch, type JSX, type SetStateAction } from 'react'

import type { AssetRowState, AssetsTableState } from '#/layouts/AssetsTable'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import type { AnyAsset, BackendType } from '#/services/Backend'
import type { SortInfo } from '#/utilities/sorting'
import type { SortableColumn } from './column/columnUtils'
import { Column } from './column/columnUtils'
import DocsColumn from './column/DocsColumn'
import LabelsColumn from './column/LabelsColumn'
import ModifiedColumn from './column/ModifiedColumn'
import NameColumn from './column/NameColumn'
import PathColumn from './column/PathColumn'
import PlaceholderColumn from './column/PlaceholderColumn'
import SharedWithColumn from './column/SharedWithColumn'

/** Props for an arbitrary variant of {@link Asset}. */
export interface AssetColumnProps {
  readonly isOpened: boolean
  readonly item: AnyAsset
  readonly depth: number
  readonly backendType: BackendType
  readonly selected: boolean
  readonly setSelected: (selected: boolean) => void
  readonly isSoleSelected: boolean
  readonly state: AssetsTableState
  readonly rowState: AssetRowState
  readonly setRowState: Dispatch<SetStateAction<AssetRowState>>
  readonly isEditable: boolean
  readonly isPlaceholder: boolean
  readonly isExpanded: boolean
}

/** Props for a {@link AssetColumn}. */
export interface AssetColumnHeadingProps {
  readonly category: Category
  readonly hideColumn: (column: Column) => void
  readonly sortInfo: SortInfo<SortableColumn> | null
  readonly setSortInfo: (sortInfo: SortInfo<SortableColumn> | null) => void
}

/** Metadata describing how to render a column of the table. */
export interface AssetColumn {
  readonly id: string
  readonly className?: string
  readonly heading: (props: AssetColumnHeadingProps) => JSX.Element
  readonly render: (props: AssetColumnProps) => JSX.Element
}

// =======================
// === COLUMN_RENDERER ===
// =======================

/** React components for every column. */
export const COLUMN_RENDERER: Readonly<
  Record<Column, React.MemoExoticComponent<(props: AssetColumnProps) => React.JSX.Element>>
> = {
  [Column.name]: memo(NameColumn),
  [Column.modified]: memo(ModifiedColumn),
  [Column.sharedWith]: memo(SharedWithColumn),
  [Column.labels]: memo(LabelsColumn),
  [Column.accessedByProjects]: memo(PlaceholderColumn),
  [Column.accessedData]: memo(PlaceholderColumn),
  [Column.docs]: memo(DocsColumn),
  [Column.path]: memo(PathColumn),
}
