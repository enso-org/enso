/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import * as textProvider from '#/providers/TextProvider'

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
  const { getText } = textProvider.useText()
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
        alt={isAssetPanelEnabled ? getText('closeAssetPanel') : getText('openAssetPanel')}
        active={isAssetPanelEnabled}
        image={SettingsIcon}
        error={getText('multipleAssetsSettingsError')}
        onClick={() => {
          setIsAssetPanelEnabled(visible => !visible)
        }}
      />
    </div>
  )
}
