/** @file A heading for the "Shared with" column. */
import * as React from 'react'

import PeopleIcon from 'enso-assets/people.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Button from '#/components/styled/Button'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-table-row w-full items-center gap-icon-with-text">
      <Button
        active
        image={PeopleIcon}
        className="size-icon"
        alt={getText('sharedWithColumnHide')}
        onPress={() => {
          hideColumn(columnUtils.Column.sharedWith)
        }}
      />
      <aria.Text className="text-header">{getText('sharedWithColumnName')}</aria.Text>
    </div>
  )
}
