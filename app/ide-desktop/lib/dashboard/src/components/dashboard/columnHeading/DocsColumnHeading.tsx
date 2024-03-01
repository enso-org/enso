/** @file A heading for the "Docs" column. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask src={DocsIcon} className="size-icon" />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.docs]}</span>
    </div>
  )
}
