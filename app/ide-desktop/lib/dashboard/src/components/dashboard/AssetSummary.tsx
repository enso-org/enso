/** @file Displays a few details of an asset. */
import * as React from 'react'

import BreadcrumbArrowIcon from 'enso-assets/breadcrumb_arrow.svg'

import type * as backend from '#/services/backend'
import * as dateTime from '#/utilities/dateTime'

import AssetIcon from '#/components/dashboard/AssetIcon'

/** Props for an {@link AssetSummary}. */
export interface AssetSummaryProps {
  asset: backend.AnyAsset
  /** If `true`, `lastModified` will be hidden, as it is not relevant. */
  new?: boolean
  newName?: string
  className?: string
}

/** Displays a few details of an asset. */
export default function AssetSummary(props: AssetSummaryProps) {
  const { asset, new: isNew = false, newName, className } = props
  return (
    <div className={`flex items-center gap-2.5 rounded-2xl bg-frame px-2 ${className}`}>
      <div className="grid place-items-center h-8 w-4">
        <AssetIcon asset={asset} />
      </div>
      <div className="flex flex-col">
        <span className="flex items-center gap-2 font-semibold">
          {asset.title}
          {newName != null && (
            <>
              <img src={BreadcrumbArrowIcon} />
              {newName}
            </>
          )}
        </span>
        {!isNew && (
          <span>last modified on {dateTime.formatDateTime(new Date(asset.modifiedAt))}</span>
        )}
        <span>{asset.labels}</span>
      </div>
    </div>
  )
}
