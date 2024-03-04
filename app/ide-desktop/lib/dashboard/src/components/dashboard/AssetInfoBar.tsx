/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import * as backendProvider from '#/providers/BackendProvider'

import Button from '#/components/Button'

import * as backendModule from '#/services/Backend'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  readonly isAssetPanelVisible: boolean
  readonly setIsAssetPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
}

/** A menubar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { isAssetPanelVisible, setIsAssetPanelVisible } = props
  const { backend } = backendProvider.useBackend()
  return (
    <div
      className={`flex items-center shrink-0 bg-frame rounded-full gap-icons h-row px-icons-x cursor-default pointer-events-auto ${
        backend.type === backendModule.BackendType.remote ? '' : 'invisible'
      }`}
      onClick={event => {
        event.stopPropagation()
      }}
    >
      {/*<Button
        active={false}
        disabled
        image={DocsIcon}
        error="Not implemented yet."
        onClick={() => {
          // No backend support yet.
        }}
      />*/}
      <Button
        alt={isAssetPanelVisible ? 'Close Asset Panel' : 'Open Asset Panel'}
        active={isAssetPanelVisible}
        image={SettingsIcon}
        error="Select exactly one asset to see its settings."
        onClick={() => {
          setIsAssetPanelVisible(visible => !visible)
        }}
      />
    </div>
  )
}
