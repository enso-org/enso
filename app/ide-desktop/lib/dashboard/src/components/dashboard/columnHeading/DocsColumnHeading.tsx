/** @file A heading for the "Docs" column. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'

import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={DocsIcon}
        className="size-icon"
        title={getText('hideThisColumn')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.docs)
        }}
      />
      <span className="text-header">{getText('docsColumnName')}</span>
    </div>
  )
}
