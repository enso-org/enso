/** @file Props types for columns. */
import type { Category } from '#/layouts/Drive/CategorySwitcher'
import type { AnyAsset, BackendType, Label } from '#/services/Backend'
import type { SortInfo } from '#/utilities/sorting'
import type { Dispatch, JSX, SetStateAction } from 'react'
import type { AssetRowState, AssetsTableState } from '../../types'
import type { Column, SortableColumn } from './types'

/** Props for an arbitrary variant of {@link Asset}. */
export interface AssetColumnProps {
  readonly isOpened: boolean
  readonly isNavigating: boolean
  readonly item: AnyAsset
  readonly labels: readonly Label[]
  readonly backendType: BackendType
  readonly setSelected: (selected: boolean) => void
  readonly state: AssetsTableState
  readonly rowState: AssetRowState
  readonly setRowState: Dispatch<SetStateAction<AssetRowState>>
  readonly isEditable: boolean
  readonly isPlaceholder: boolean
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
