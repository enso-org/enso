/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={TagIcon}
        className="size-icon"
        title={getText('labelsColumnHide')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.labels)
        }}
      />
      <span className="text-header">{getText('labelsColumnName')}</span>
    </div>
  )
}
