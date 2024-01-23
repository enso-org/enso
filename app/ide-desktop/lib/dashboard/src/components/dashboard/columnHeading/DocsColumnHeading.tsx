/** @file A heading for the "Docs" column. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

/** A heading for the "Docs" column. */
export default function DocsColumnHeading(): JSX.Element {
  const { getText } = textProvider.useText()
  return (
    <div className="flex items-center gap-2">
      <SvgMask src={DocsIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">{getText('docsColumnName')}</span>
    </div>
  )
}
