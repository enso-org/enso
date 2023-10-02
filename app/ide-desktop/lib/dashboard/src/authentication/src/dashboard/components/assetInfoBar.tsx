/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import * as backend from '../backend'
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
        <div className="flex items-center shrink-0 bg-frame backdrop-blur-3xl rounded-full gap-3 h-8 px-2 cursor-default pointer-events-auto">
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
