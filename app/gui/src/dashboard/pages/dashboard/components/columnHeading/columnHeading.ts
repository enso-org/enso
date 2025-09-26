/** @file A lookup containing a component for the corresponding heading for each column type. */
import { memo } from 'react'
import type * as column from '../column'
import type { Column } from '../column/columnUtils'
import {
  AccessedByProjectsColumnHeading,
  AccessedDataColumnHeading,
  LabelsColumnHeading,
  ModifiedColumnHeading,
  NameColumnHeading,
  PathColumnHeading,
  SharedWithColumnHeading,
} from './components'

export const COLUMN_HEADING: Readonly<
  Record<
    Column,
    React.MemoExoticComponent<(props: column.AssetColumnHeadingProps) => React.JSX.Element>
  >
> = {
  ['name']: memo(NameColumnHeading),
  ['modified']: memo(ModifiedColumnHeading),
  ['sharedWith']: memo(SharedWithColumnHeading),
  ['labels']: memo(LabelsColumnHeading),
  ['accessedByProjects']: memo(AccessedByProjectsColumnHeading),
  ['accessedData']: memo(AccessedDataColumnHeading),
  ['path']: memo(PathColumnHeading),
}
