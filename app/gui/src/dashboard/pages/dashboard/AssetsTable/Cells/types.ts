/**
 * @file Types for assets table cells
 */

import type { AnyAsset } from '#/services/Backend'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import type { CellContext } from '@tanstack/react-table'
import type { EditingNameRowState } from './assetsTableFeatures'

/**
 * Cell context for assets table
 */
export type AssetsTableCellContext = CellContext<AssetTreeNode<AnyAsset>, string> &
  EditingNameRowState

/**
 * Generic props for any cell
 */
export interface AnyCellProps {
  readonly state: AssetsTableCellContext
}
