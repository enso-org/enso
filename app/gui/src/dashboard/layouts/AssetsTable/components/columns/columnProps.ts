/** @file Props types for columns. */
import type { AssetRowState, AssetsTableState } from '#/layouts/AssetsTable/types'
import type { Category } from '#/layouts/Drive/CategorySwitcher'
import type { LaunchedProject } from '#/providers/ProjectsProvider'
import type { AnyAsset, AssetId, BackendType, Label, ProjectId } from '#/services/Backend'
import type { SortInfo } from '#/utilities/sorting'
import { type Dispatch, type JSX, type SetStateAction } from 'react'
import type { Column, SortableColumn } from './types'

/** Props for an arbitrary variant of {@link Asset}. */
export interface AssetColumnProps {
  readonly isOpened: boolean
  readonly isNavigating: boolean
  readonly item: AnyAsset
  readonly backendType: BackendType
  readonly setSelected: (selected: boolean) => void
  readonly state: AssetsTableState
  readonly rowState: AssetRowState
  readonly setRowState: Dispatch<SetStateAction<AssetRowState>>
  readonly isEditable: boolean
  readonly isPlaceholder: boolean
  readonly labels: readonly Label[]
  readonly renameAsset: (assetId: AssetId, newTitle: string) => Promise<void>
  readonly closeProject: (project: LaunchedProject) => Promise<void>
  readonly openProject: (projectId: ProjectId) => Promise<void>
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
