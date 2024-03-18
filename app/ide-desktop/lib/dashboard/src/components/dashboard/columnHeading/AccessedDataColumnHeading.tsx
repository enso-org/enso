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
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={AccessedDataIcon}
        className="size-icon"
        alt="Hide Accessed data"
        title="Hide Accessed data"
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
