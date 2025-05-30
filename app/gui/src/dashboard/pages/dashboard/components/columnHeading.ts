/** @file A lookup containing a component for the corresponding heading for each column type. */
import { memo } from 'react'
import type * as column from './column'
import { Column } from './column/columnUtils'
import AccessedByProjectsColumnHeading from './columnHeading/AccessedByProjectsColumnHeading'
import AccessedDataColumnHeading from './columnHeading/AccessedDataColumnHeading'
import LabelsColumnHeading from './columnHeading/LabelsColumnHeading'
import ModifiedColumnHeading from './columnHeading/ModifiedColumnHeading'
import NameColumnHeading from './columnHeading/NameColumnHeading'
import PathColumnHeading from './columnHeading/PathColumnHeading'
import SharedWithColumnHeading from './columnHeading/SharedWithColumnHeading'

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
