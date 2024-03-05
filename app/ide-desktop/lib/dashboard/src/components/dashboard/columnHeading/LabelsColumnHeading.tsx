/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state

  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask
        src={TagIcon}
        className="size-icon"
        title="Hide this column"
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.labels)
        }}
      />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.labels]}</span>
    </div>
  )
}
