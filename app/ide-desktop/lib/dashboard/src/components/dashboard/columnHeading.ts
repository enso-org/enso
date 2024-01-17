/** @file A lookup containing a component for the corresponding heading for each column type. */
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import AccessedByProjectsColumnHeading from '#/components/dashboard/columnHeading/AccessedByProjectsColumnHeading'
import AccessedDataColumnHeading from '#/components/dashboard/columnHeading/AccessedDataColumnHeading'
import DocsColumnHeading from '#/components/dashboard/columnHeading/DocsColumnHeading'
import LabelsColumnHeading from '#/components/dashboard/columnHeading/LabelsColumnHeading'
import ModifiedColumnHeading from '#/components/dashboard/columnHeading/ModifiedColumnHeading'
import NameColumnHeading from '#/components/dashboard/columnHeading/NameColumnHeading'
import SharedWithColumnHeading from '#/components/dashboard/columnHeading/SharedWithColumnHeading'

export const COLUMN_HEADING: Record<
    columnUtils.Column,
    (props: column.AssetColumnHeadingProps) => JSX.Element
> = {
    [columnUtils.Column.name]: NameColumnHeading,
    [columnUtils.Column.modified]: ModifiedColumnHeading,
    [columnUtils.Column.sharedWith]: SharedWithColumnHeading,
    [columnUtils.Column.labels]: LabelsColumnHeading,
    [columnUtils.Column.accessedByProjects]: AccessedByProjectsColumnHeading,
    [columnUtils.Column.accessedData]: AccessedDataColumnHeading,
    [columnUtils.Column.docs]: DocsColumnHeading,
}
