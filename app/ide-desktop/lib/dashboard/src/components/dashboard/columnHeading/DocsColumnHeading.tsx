/** @file A heading for the "Docs" column. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-2">
      <SvgMask src={DocsIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">
        {columnUtils.COLUMN_NAME[columnUtils.Column.docs]}
      </span>
    </div>
  )
}
