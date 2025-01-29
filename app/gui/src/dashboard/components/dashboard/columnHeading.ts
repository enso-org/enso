/** @file A lookup containing a component for the corresponding heading for each column type. */
import { memo } from 'react'
import type * as column from './column'
import * as columnUtils from './column/columnUtils'
import AccessedByProjectsColumnHeading from './columnHeading/AccessedByProjectsColumnHeading'
import AccessedDataColumnHeading from './columnHeading/AccessedDataColumnHeading'
import DocsColumnHeading from './columnHeading/DocsColumnHeading'
import LabelsColumnHeading from './columnHeading/LabelsColumnHeading'
import ModifiedColumnHeading from './columnHeading/ModifiedColumnHeading'
import NameColumnHeading from './columnHeading/NameColumnHeading'
import PathColumnHeading from './columnHeading/PathColumnHeading'
import SharedWithColumnHeading from './columnHeading/SharedWithColumnHeading'

export const COLUMN_HEADING: Readonly<
  Record<
    columnUtils.Column,
    React.MemoExoticComponent<(props: column.AssetColumnHeadingProps) => React.JSX.Element>
  >
> = {
  [columnUtils.Column.name]: memo(NameColumnHeading),
  [columnUtils.Column.modified]: memo(ModifiedColumnHeading),
  [columnUtils.Column.sharedWith]: memo(SharedWithColumnHeading),
  [columnUtils.Column.labels]: memo(LabelsColumnHeading),
  [columnUtils.Column.accessedByProjects]: memo(AccessedByProjectsColumnHeading),
  [columnUtils.Column.accessedData]: memo(AccessedDataColumnHeading),
  [columnUtils.Column.docs]: memo(DocsColumnHeading),
  [columnUtils.Column.path]: memo(PathColumnHeading),
}
