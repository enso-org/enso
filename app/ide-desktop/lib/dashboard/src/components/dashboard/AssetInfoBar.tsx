/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import DocsIcon from 'enso-assets/docs.svg'
import SettingsIcon from 'enso-assets/settings.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/Button'

import * as backendModule from '#/services/Backend'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  readonly canToggleAssetPanel: boolean
  readonly isAssetPanelVisible: boolean
  readonly setIsAssetPanelVisible: React.Dispatch<React.SetStateAction<boolean>>
}

/** A toolbar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { canToggleAssetPanel, isAssetPanelVisible, setIsAssetPanelVisible } = props
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  return (
    <div
      className={`flex items-center shrink-0 bg-frame rounded-full gap-3 h-8 px-2 cursor-default pointer-events-auto ${
        backend.type === backendModule.BackendType.remote ? '' : 'invisible'
      }`}
      onClick={event => {
        event.stopPropagation()
      }}
    >
      <Button
        active={false}
        disabled
        image={DocsIcon}
        error={getText('notImplemetedYet')}
        onClick={() => {
          // No backend support yet.
        }}
      />
      <Button
        alt={isAssetPanelVisible ? getText('closeAssetPanel') : getText('openAssetPanel')}
        active={canToggleAssetPanel && isAssetPanelVisible}
        disabled={!canToggleAssetPanel}
        image={SettingsIcon}
        error={getText('multipleAssetsSettingsError')}
        onClick={() => {
          setIsAssetPanelVisible(visible => !visible)
        }}
      />
    </div>
  )
}
