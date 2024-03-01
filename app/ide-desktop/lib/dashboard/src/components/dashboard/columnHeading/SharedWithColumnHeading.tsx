/** @file A heading for the "Shared with" column. */
import * as React from 'react'

import PeopleIcon from 'enso-assets/people.svg'

import * as columnUtils from '#/components/dashboard/column/columnUtils'
import SvgMask from '#/components/SvgMask'

/** A heading for the "Shared with" column. */
export default function SharedWithColumnHeading(): JSX.Element {
  return (
    <div className="flex items-center gap-icon-with-text w-full h-drive-table-heading">
      <SvgMask src={PeopleIcon} className="size-icon" />
      <span className="text-header">{columnUtils.COLUMN_NAME[columnUtils.Column.sharedWith]}</span>
    </div>
  )
}
