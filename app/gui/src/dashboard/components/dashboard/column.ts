/** @file Column types and column display modes. */
import type { Dispatch, JSX, SetStateAction } from 'react'

import { Column } from '#/components/dashboard/column/columnUtils'
import DocsColumn from '#/components/dashboard/column/DocsColumn'
import LabelsColumn from '#/components/dashboard/column/LabelsColumn'
import ModifiedColumn from '#/components/dashboard/column/ModifiedColumn'
import NameColumn from '#/components/dashboard/column/NameColumn'
import PlaceholderColumn from '#/components/dashboard/column/PlaceholderColumn'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import type { AssetRowState, AssetsTableState } from '#/layouts/AssetsTable'
import type { AnyAsset, Asset, AssetId, BackendType } from '#/services/Backend'

// ===================
// === AssetColumn ===
// ===================

/** Props for an arbitrary variant of {@link Asset}. */
export interface AssetColumnProps {
  readonly keyProp: AssetId
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
}

/** Props for a {@link AssetColumn}. */
export interface AssetColumnHeadingProps {
  readonly state: AssetsTableState
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
export const COLUMN_RENDERER: Readonly<Record<Column, (props: AssetColumnProps) => JSX.Element>> = {
  [Column.name]: NameColumn,
  [Column.modified]: ModifiedColumn,
  [Column.sharedWith]: SharedWithColumn,
  [Column.labels]: LabelsColumn,
  [Column.accessedByProjects]: PlaceholderColumn,
  [Column.accessedData]: PlaceholderColumn,
  [Column.docs]: DocsColumn,
}
