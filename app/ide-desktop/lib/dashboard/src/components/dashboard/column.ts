/** @file Column types and column display modes. */
import type * as assetsTable from '#/layouts/dashboard/AssetsTable'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import DocsColumn from '#/components/dashboard/column/DocsColumn'
import LabelsColumn from '#/components/dashboard/column/LabelsColumn'
import ModifiedColumn from '#/components/dashboard/column/ModifiedColumn'
import NameColumn from '#/components/dashboard/column/NameColumn'
import PlaceholderColumn from '#/components/dashboard/column/PlaceholderColumn'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'

import type * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

// ===================
// === AssetColumn ===
// ===================

/** Props for an arbitrary variant of {@link backendModule.Asset}. */
export interface AssetColumnProps {
  readonly keyProp: backendModule.AssetId
  readonly item: AssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<AssetTreeNode>>
  readonly selected: boolean
  readonly setSelected: (selected: boolean) => void
  readonly isSoleSelectedItem: boolean
  readonly state: assetsTable.AssetsTableState
  readonly rowState: assetsTable.AssetRowState
  readonly setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for a {@link AssetColumn}. */
export interface AssetColumnHeadingProps {
  readonly state: assetsTable.AssetsTableState
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
  Record<columnUtils.Column, (props: AssetColumnProps) => JSX.Element>
> = {
  [columnUtils.Column.name]: NameColumn,
  [columnUtils.Column.modified]: ModifiedColumn,
  [columnUtils.Column.sharedWith]: SharedWithColumn,
  [columnUtils.Column.labels]: LabelsColumn,
  [columnUtils.Column.accessedByProjects]: PlaceholderColumn,
  [columnUtils.Column.accessedData]: PlaceholderColumn,
  [columnUtils.Column.docs]: DocsColumn,
}
