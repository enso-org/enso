/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import type * as backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  number: number
  version: backend.S3ObjectVersion
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version } = props

  return (
    <div className="flex flex-col cursor-pointer rounded-2xl p-2 select-none overflow-y-auto hover:bg-frame transition-colors">
      <div>version {number}</div>
      <div className="text-not-selected text-xs">
        on {dateTime.formatDateTime(new Date(version.lastModified))}
      </div>
    </div>
  )
}
