/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state

  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask
        src={AccessedDataIcon}
        className="size-icon"
        title="Hide this column"
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.accessedData)
        }}
      />
      <span className="text-header">
        {columnUtils.COLUMN_NAME[columnUtils.Column.accessedData]}
      </span>
    </div>
  )
}
