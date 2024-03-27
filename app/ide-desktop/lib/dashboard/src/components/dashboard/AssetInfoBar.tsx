/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

import * as backendModule from '#/services/Backend'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
}

/** A menubar for displaying asset information. */
// This parameter will be used in the future.
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { invisible = false, isAssetPanelEnabled, setIsAssetPanelEnabled } = props
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {(ref, innerProps) => (
        <div
          ref={ref}
          className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-icons rounded-full bg-frame px-icons-x ${
            backend.type === backendModule.BackendType.remote ? '' : 'invisible'
          }`}
          {...innerProps}
        >
          <Button
            alt={isAssetPanelEnabled ? getText('closeAssetPanel') : getText('openAssetPanel')}
            active={isAssetPanelEnabled}
            image={SettingsIcon}
            error={getText('multipleAssetsSettingsError')}
            onPress={() => {
              setIsAssetPanelEnabled(visible => !visible)
            }}
          />
        </div>
      )}
    </FocusArea>
  )
}
