/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <Button
        active
        image={TagIcon}
        className="size-icon"
        alt={getText('labelsColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.labels)
        }}
      />
      <aria.Text className="text-header">{getText('labelsColumnName')}</aria.Text>
    </div>
  )
}
