/** @file Column types and column display modes. */

import type * as assetsTable from '#/layouts/dashboard/assetsTable'
import type * as backendModule from '#/services/backend'
import type * as assetTreeNode from '#/util/assetTreeNode'

import AssetNameColumn from '#/components/dashboard/assetNameColumn'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import DocsColumn from '#/components/dashboard/column/docsColumn'
import LabelsColumn from '#/components/dashboard/column/labelsColumn'
import LastModifiedColumn from '#/components/dashboard/column/lastModifiedColumn'
import PlaceholderColumn from '#/components/dashboard/column/placeholderColumn'
import SharedWithColumn from '#/components/dashboard/column/sharedWithColumn'
import type * as tableColumn from '#/components/tableColumn'

// ==========================
// === LastModifiedColumn ===
// ==========================

/** {@link tableColumn.TableColumnProps} for an unknown variant of {@link backendModule.Asset}. */
export type AssetColumnProps = tableColumn.TableColumnProps<
    assetTreeNode.AssetTreeNode,
    assetsTable.AssetsTableState,
    assetsTable.AssetRowState,
    backendModule.AssetId
>

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<columnUtils.Column, (props: AssetColumnProps) => JSX.Element> =
    {
        [columnUtils.Column.name]: AssetNameColumn,
        [columnUtils.Column.modified]: LastModifiedColumn,
        [columnUtils.Column.sharedWith]: SharedWithColumn,
        [columnUtils.Column.labels]: LabelsColumn,
        [columnUtils.Column.accessedByProjects]: PlaceholderColumn,
        [columnUtils.Column.accessedData]: PlaceholderColumn,
        [columnUtils.Column.docs]: DocsColumn,
    }
