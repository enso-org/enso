/** @file A heading for the "Accessed by projects" column. */
import * as React from 'react'

import AccessedByProjectsIcon from 'enso-assets/accessed_by_projects.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed by projects" column. */
export default function AccessedByProjectsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state

  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask
        src={AccessedByProjectsIcon}
        className="size-icon"
        title="Hide this column"
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.accessedByProjects)
        }}
      />
      <span className="text-header">
        {columnUtils.COLUMN_NAME[columnUtils.Column.accessedByProjects]}
      </span>
    </div>
  )
}
