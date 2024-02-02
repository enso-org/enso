/** @file Column types and column display modes. */
import type * as assetsTable from '#/layouts/dashboard/AssetsTable'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import DocsColumn from '#/components/dashboard/column/DocsColumn'
import LabelsColumn from '#/components/dashboard/column/LabelsColumn'
import LastModifiedColumn from '#/components/dashboard/column/LastModifiedColumn'
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
  keyProp: backendModule.AssetId
  item: AssetTreeNode
  setItem: React.Dispatch<React.SetStateAction<AssetTreeNode>>
  selected: boolean
  setSelected: (selected: boolean) => void
  isSoleSelectedItem: boolean
  state: assetsTable.AssetsTableState
  rowState: assetsTable.AssetRowState
  setRowState: React.Dispatch<React.SetStateAction<assetsTable.AssetRowState>>
}

/** Props for a {@link AssetColumn}. */
export interface AssetColumnHeadingProps {
  state: assetsTable.AssetsTableState
}

/** Metadata describing how to render a column of the table. */
export interface AssetColumn {
  id: string
  className?: string
  heading: (props: AssetColumnHeadingProps) => JSX.Element
  render: (props: AssetColumnProps) => JSX.Element
}

// =======================
// === COLUMN_RENDERER ===
// =======================

/** React components for every column. */
export const COLUMN_RENDERER: Record<columnUtils.Column, (props: AssetColumnProps) => JSX.Element> =
  {
    [columnUtils.Column.name]: NameColumn,
    [columnUtils.Column.modified]: LastModifiedColumn,
    [columnUtils.Column.sharedWith]: SharedWithColumn,
    [columnUtils.Column.labels]: LabelsColumn,
    [columnUtils.Column.accessedByProjects]: PlaceholderColumn,
    [columnUtils.Column.accessedData]: PlaceholderColumn,
    [columnUtils.Column.docs]: DocsColumn,
  }
