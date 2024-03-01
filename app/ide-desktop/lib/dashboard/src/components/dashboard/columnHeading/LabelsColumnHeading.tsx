/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask src={TagIcon} className="size-icon" />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.labels]}</span>
    </div>
  )
}
