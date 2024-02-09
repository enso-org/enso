/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-2">
      <SvgMask src={AccessedDataIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">
        {columnUtils.COLUMN_NAME[columnUtils.Column.accessedData]}
      </span>
    </div>
  )
}
