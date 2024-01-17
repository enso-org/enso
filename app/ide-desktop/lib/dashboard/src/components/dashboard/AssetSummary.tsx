/** @file Displays a few details of an asset. */
import * as React from 'react'

import type * as backend from '#/services/backend'
import * as dateTime from '#/utilities/dateTime'

import AssetIcon from '#/components/dashboard/AssetIcon'

/** Props for an {@link AssetSummary}. */
export interface AssetSummaryProps {
    asset: backend.AnyAsset
    className?: string
}

/** Displays a few details of an asset. */
export default function AssetSummary(props: AssetSummaryProps) {
    const { asset, className } = props
    return (
        <div className={`flex gap-2.5 rounded-2xl bg-frame ${className}`}>
            <AssetIcon asset={asset} className="h-8 w-8" />
            <div className="flex flex-col">
                <span className="font-semibold">{asset.title}</span>
                <span>last modified on {dateTime.formatDateTime(new Date(asset.modifiedAt))}</span>
                <span>{asset.labels}</span>
            </div>
        </div>
    )
}
