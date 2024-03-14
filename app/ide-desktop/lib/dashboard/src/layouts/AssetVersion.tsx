/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import type * as backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly number: number
  readonly version: backend.S3ObjectVersion
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version } = props
  const { getText } = textProvider.useText()

  return (
    <div className="flex cursor-pointer select-none flex-col overflow-y-auto rounded-default p-version transition-colors hover:bg-frame">
      <div>{getText('versionX', String(number))}</div>
      <div className="text-xs text-not-selected">
        {getText('onDateX', dateTime.formatDateTime(new Date(version.lastModified)))}
      </div>
    </div>
  )
}
