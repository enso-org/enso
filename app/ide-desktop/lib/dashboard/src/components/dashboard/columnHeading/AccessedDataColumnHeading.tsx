/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={AccessedDataIcon}
        className="size-icon"
        alt={getText('accessedDataColumnHide')}
        title={getText('accessedDataColumnHide')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.accessedData)
        }}
      />
      <aria.Text className="text-header">{getText('accessedDataColumnName')}</aria.Text>
    </div>
  )
}
