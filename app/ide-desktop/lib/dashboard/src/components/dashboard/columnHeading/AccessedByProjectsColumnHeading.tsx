/** @file A heading for the "Accessed by projects" column. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask src={AccessedByProjectsIcon} className="size-icon" />
      <span className="text-header">
        {columnUtils.COLUMN_NAME[columnUtils.Column.accessedByProjects]}
      </span>
    </div>
  )
}
