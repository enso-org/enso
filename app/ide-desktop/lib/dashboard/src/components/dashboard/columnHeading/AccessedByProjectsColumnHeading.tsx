/** @file A heading for the "Accessed by projects" column. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(): JSX.Element {
    return (
        <div className="flex items-center gap-2">
            <SvgMask src={AccessedByProjectsIcon} className="h-4 w-4" />
            <span className="leading-144.5 h-6 py-0.5">
                {columnUtils.COLUMN_NAME[columnUtils.Column.accessedByProjects]}
            </span>
        </div>
    )
}
