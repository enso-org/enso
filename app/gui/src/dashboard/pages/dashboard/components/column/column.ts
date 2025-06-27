/** @file Column types and column display modes. */
import type { AssetsTableState } from '#/layouts/AssetsTable'
import type { Category } from '#/layouts/CategorySwitcher/Category'
import type { AnyAsset, BackendType } from '#/services/Backend'
import type { SortInfo } from '#/utilities/sorting'
import { memo, type JSX } from 'react'
import type { SortableColumn } from './columnUtils'
import { Column } from './columnUtils'
import {
  LabelsColumn,
  ModifiedColumn,
  NameColumn,
  PathColumn,
  PlaceholderColumn,
  SharedWithColumn,
} from './components'
export * from './components'

/** Props for an arbitrary variant of {@link Asset}. */
export interface AssetColumnProps {
  readonly isNavigating: boolean
  readonly item: AnyAsset
  readonly backendType: BackendType
  readonly state: AssetsTableState
  readonly isEditable: boolean
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
  [Column.path]: memo(PathColumn),
}
