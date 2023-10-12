/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import type * as backend from '../backend'
import Button from './button'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
    asset: backend.Asset | null
}

/** A toolbar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(_props: AssetInfoBarProps) {
    return (
        <div className="pointer-events-auto flex h-8 shrink-0 cursor-default items-center gap-3 rounded-full bg-frame px-2 backdrop-blur-3xl">
            <Button
                active={false}
                disabled
                image={DocsIcon}
                error="Not implemented yet."
                onClick={() => {
                    // No backend support yet.
                }}
            />
            <Button
                active={false}
                disabled
                image={SettingsIcon}
                error="Not implemented yet."
                onClick={() => {
                    // No backend support yet.
                }}
            />
        </div>
    )
}
