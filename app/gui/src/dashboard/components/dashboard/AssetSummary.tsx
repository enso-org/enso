/** @file Displays a few details of an asset. */
import * as React from 'react'

import BreadcrumbArrowIcon from '#/assets/breadcrumb_arrow.svg'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import AssetIcon from '#/components/dashboard/AssetIcon'

import type * as backend from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// ====================
// === AssetSummary ===
// ====================

/** Props for an {@link AssetSummary}. */
export interface AssetSummaryProps {
  readonly asset: backend.AnyAsset
  /** If `true`, `lastModified` will be hidden, as it is not relevant. */
  readonly new?: boolean
  readonly newName?: string
  readonly className?: string
}

/** Displays a few details of an asset. */
export default function AssetSummary(props: AssetSummaryProps) {
  const { asset, new: isNew = false, newName, className } = props
  const { getText } = textProvider.useText()
  return (
    <div
      className={tailwindMerge.twMerge(
        'flex min-h-row items-center gap-icon-with-text rounded-default bg-frame px-button-x',
        className,
      )}
    >
      <div className="grid size-4 place-items-center">
        <AssetIcon asset={asset} />
      </div>
      <div className="flex flex-col">
        <aria.Text className="flex items-center gap-icon-with-text font-semibold">
          {asset.title}
          {newName != null && (
            <>
              <img src={BreadcrumbArrowIcon} />
              {newName}
            </>
          )}
        </aria.Text>
        {!isNew && (
          <aria.Text>
            {getText('lastModifiedOn', dateTime.formatDateTime(new Date(asset.modifiedAt)))}
          </aria.Text>
        )}
        <aria.Text>{asset.labels}</aria.Text>
      </div>
    </div>
  )
}
