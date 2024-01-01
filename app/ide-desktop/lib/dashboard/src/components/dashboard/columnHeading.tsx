/** @file A lookup containing a component for the corresponding heading for each column type. */

import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'
import AccessedDataIcon from 'enso-assets/accessed_data.svg'
import DocsIcon from 'enso-assets/docs.svg'
import PeopleIcon from 'enso-assets/people.svg'
import SortAscendingIcon from 'enso-assets/sort_ascending.svg'
import TagIcon from 'enso-assets/tag.svg'
import TimeIcon from 'enso-assets/time.svg'

import type * as assetsTable from '#/layouts/dashboard/assetsTable'
import * as sorting from '#/util/sorting'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/svgMask'
import type * as tableColumn from '#/components/tableColumn'

/** A heading for the "Name" column. */
function NameColumnHeading(
    props: tableColumn.TableColumnHeadingProps<assetsTable.AssetsTableState>
): JSX.Element {
    const {
        state: { sortColumn, setSortColumn, sortDirection, setSortDirection },
    } = props
    const [isHovered, setIsHovered] = React.useState(false)
    const isSortActive = sortColumn === columnUtils.Column.name && sortDirection != null
    return (
        <div
            className="flex items-center cursor-pointer gap-2 pt-1 pb-1.5"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
            onClick={event => {
                event.stopPropagation()
                if (sortColumn === columnUtils.Column.name) {
                    setSortDirection(sorting.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
                } else {
                    setSortColumn(columnUtils.Column.name)
                    setSortDirection(sorting.SortDirection.ascending)
                }
            }}
        >
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.name]}
            </span>
            <img
                src={isSortActive ? columnUtils.SORT_ICON[sortDirection] : SortAscendingIcon}
                className={isSortActive ? '' : isHovered ? 'opacity-50' : 'opacity-0'}
            />
        </div>
    )
}

/** A heading for the "Modified" column. */
function ModifiedColumnHeading(
    props: tableColumn.TableColumnHeadingProps<assetsTable.AssetsTableState>
): JSX.Element {
    const {
        state: { sortColumn, setSortColumn, sortDirection, setSortDirection },
    } = props
    const [isHovered, setIsHovered] = React.useState(false)
    const isSortActive = sortColumn === columnUtils.Column.modified && sortDirection != null
    return (
        <div
            className="flex items-center cursor-pointer gap-2"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
            onClick={event => {
                event.stopPropagation()
                if (sortColumn === columnUtils.Column.modified) {
                    setSortDirection(sorting.NEXT_SORT_DIRECTION[sortDirection ?? 'null'])
                } else {
                    setSortColumn(columnUtils.Column.modified)
                    setSortDirection(sorting.SortDirection.ascending)
                }
            }}
        >
            <SvgMask src={TimeIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.modified]}
            </span>
            <img
                src={isSortActive ? columnUtils.SORT_ICON[sortDirection] : SortAscendingIcon}
                className={isSortActive ? '' : isHovered ? 'opacity-50' : 'opacity-0'}
            />
        </div>
    )
}

/** A heading for the "Shared With" column. */
function SharedWithColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={PeopleIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.sharedWith]}
            </span>
        </div>
    )
}

/** A heading for the "Labels" column. */
function LabelsColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={TagIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.labels]}
            </span>
        </div>
    )
}

/** A heading for the "Accessed By Projects" column. */
function AccessedByProjectsColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={AccessedByProjectsIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.accessedByProjects]}
            </span>
        </div>
    )
}

/** A heading for the "Accessed Data" column. */
function AccessedDataColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={AccessedDataIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.accessedData]}
            </span>
        </div>
    )
}

/** A heading for the "Docs" column. */
function DocsColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={DocsIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.docs]}
            </span>
        </div>
    )
}

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
