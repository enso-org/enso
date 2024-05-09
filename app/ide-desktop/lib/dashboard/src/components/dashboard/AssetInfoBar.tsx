/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import SettingsIcon from 'enso-assets/settings.svg'

import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps {
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly hidden: boolean
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
}

/** A menubar for displaying asset information. */
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { invisible = false, hidden, isAssetPanelEnabled, setIsAssetPanelEnabled } = props
  const { getText } = textProvider.useText()

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {innerProps => (
        <div
          className={`pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-icons rounded-full bg-frame px-icons-x ${
            hidden ? 'invisible' : ''
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
