/** @file A heading for the "Shared with" column. */
import * as React from 'react'

import PeopleIcon from 'enso-assets/people.svg'

import * as textProvider from '#/providers/TextProvider'

import type * as column from '#/components/dashboard/column'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(props: column.AssetColumnHeadingProps) {
  const { state } = props
  const { hideColumn } = state
  const { getText } = textProvider.useText()

  return (
    <div className="flex h-drive-table-heading w-full items-center gap-icon-with-text">
      <SvgMask
        src={PeopleIcon}
        className="size-icon"
        title={getText('hideThisColumn')}
        onClick={event => {
          event.stopPropagation()
          hideColumn(columnUtils.Column.sharedWith)
        }}
      />
      <span className="text-header">{getText('sharedWithColumnName')}</span>
    </div>
  )
}
