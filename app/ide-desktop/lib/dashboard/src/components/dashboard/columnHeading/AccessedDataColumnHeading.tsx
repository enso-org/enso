/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        active
        image={AccessedDataIcon}
        className="size-icon"
        alt={getText('accessedDataColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.accessedData)
        }}
      />
      <aria.Text className="text-header">{getText('accessedDataColumnName')}</aria.Text>
    </div>
  )
}
