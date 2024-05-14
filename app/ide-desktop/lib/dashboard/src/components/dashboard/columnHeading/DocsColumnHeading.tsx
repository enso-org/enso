/** @file A heading for the "Docs" column. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        active
        image={DocsIcon}
        className="size-icon"
        alt={getText('docsColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.docs)
        }}
      />
      <aria.Text className="text-header">{getText('docsColumnName')}</aria.Text>
    </div>
  )
}
