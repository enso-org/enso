/** @file A lookup containing a component for the corresponding heading for each column type. */
import { memo } from 'react'
import type * as column from '../column'
import { Column } from '../column/columnUtils'
import AccessedByProjectsColumnHeading from './AccessedByProjectsColumnHeading'
import AccessedDataColumnHeading from './AccessedDataColumnHeading'
import LabelsColumnHeading from './LabelsColumnHeading'
import ModifiedColumnHeading from './ModifiedColumnHeading'
import NameColumnHeading from './NameColumnHeading'
import PathColumnHeading from './PathColumnHeading'
import SharedWithColumnHeading from './SharedWithColumnHeading'

export const COLUMN_HEADING: Readonly<
  Record<
    Column,
    React.MemoExoticComponent<(props: column.AssetColumnHeadingProps) => React.JSX.Element>
  >
> = {
  [Column.name]: memo(NameColumnHeading),
  [Column.modified]: memo(ModifiedColumnHeading),
  [Column.sharedWith]: memo(SharedWithColumnHeading),
  [Column.labels]: memo(LabelsColumnHeading),
  [Column.accessedByProjects]: memo(AccessedByProjectsColumnHeading),
  [Column.accessedData]: memo(AccessedDataColumnHeading),
  [Column.path]: memo(PathColumnHeading),
}
