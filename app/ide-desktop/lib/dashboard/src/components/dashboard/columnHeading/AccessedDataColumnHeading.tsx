/** @file A heading for the "Accessed data" column. */
import * as React from 'react'

import AccessedDataIcon from 'enso-assets/accessed_data.svg'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

/** A heading for the "Accessed data" column. */
export default function AccessedDataColumnHeading(): JSX.Element {
  const { getText } = textProvider.useText()
  return (
    <div className="flex items-center gap-2">
      <SvgMask src={AccessedDataIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">{getText('accessedDataColumnName')}</span>
    </div>
  )
}
