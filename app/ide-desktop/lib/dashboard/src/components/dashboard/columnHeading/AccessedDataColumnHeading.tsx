/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask src={AccessedDataIcon} className="size-icon" />
      <span className="text-header">
        {columnUtils.COLUMN_NAME[columnUtils.Column.accessedData]}
      </span>
    </div>
  )
}
