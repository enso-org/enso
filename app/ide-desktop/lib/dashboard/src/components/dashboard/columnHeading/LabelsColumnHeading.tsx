/** @file A heading for the "Labels" column. */
import * as React from 'react'

import TagIcon from 'enso-assets/tag.svg'

import * as textProvider from '#/providers/TextProvider'

import SvgMask from '#/components/SvgMask'

/** A heading for the "Labels" column. */
export default function LabelsColumnHeading(): JSX.Element {
  const { getText } = textProvider.useText()
  return (
    <div className="flex items-center gap-2">
      <SvgMask src={TagIcon} className="h-4 w-4" />
      <span className="leading-144.5 h-6 py-0.5">{getText('labelsColumnName')}</span>
    </div>
  )
}
