/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import Button from '#/components/Button'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
  readonly isCloud: boolean
}

/** A menubar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { isAssetPanelEnabled, setIsAssetPanelEnabled, isCloud } = props

  return (
    <div
      className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-icons rounded-full bg-frame px-icons-x ${
        isCloud ? '' : 'invisible'
      }`}
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <Button
        alt={isAssetPanelEnabled ? 'Close Asset Panel' : 'Open Asset Panel'}
        active={isAssetPanelEnabled}
        image={SettingsIcon}
        error="Select exactly one asset to see its settings."
        onClick={() => {
          setIsAssetPanelEnabled(visible => !visible)
        }}
      />
    </div>
  )
}
