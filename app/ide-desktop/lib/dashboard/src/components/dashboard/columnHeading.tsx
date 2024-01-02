/** @file A lookup containing a component for the corresponding heading for each column type. */

import type * as assetsTable from '#/layouts/dashboard/assetsTable'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import AccessedByProjectsColumnHeading from '#/components/dashboard/columnHeading/accessedByProjectsColumnHeading'
import AccessedDataColumnHeading from '#/components/dashboard/columnHeading/accessedDataColumnHeading'
import DocsColumnHeading from '#/components/dashboard/columnHeading/docsColumnHeading'
import LabelsColumnHeading from '#/components/dashboard/columnHeading/labelsColumnHeading'
import ModifiedColumnHeading from '#/components/dashboard/columnHeading/modifiedColumnHeading'
import NameColumnHeading from '#/components/dashboard/columnHeading/nameColumnHeading'
import SharedWithColumnHeading from '#/components/dashboard/columnHeading/sharedWithColumnHeading'
import type * as tableColumn from '#/components/tableColumn'

export const COLUMN_HEADING: Record<
    columnUtils.Column,
    (props: tableColumn.TableColumnHeadingProps<assetsTable.AssetsTableState>) => JSX.Element
> = {
    [columnUtils.Column.name]: NameColumnHeading,
    [columnUtils.Column.modified]: ModifiedColumnHeading,
    [columnUtils.Column.sharedWith]: SharedWithColumnHeading,
    [columnUtils.Column.labels]: LabelsColumnHeading,
    [columnUtils.Column.accessedByProjects]: AccessedByProjectsColumnHeading,
    [columnUtils.Column.accessedData]: AccessedDataColumnHeading,
    [columnUtils.Column.docs]: DocsColumnHeading,
}
