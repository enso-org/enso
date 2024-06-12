/** @file A toolbar for displaying asset information. */
import * as React from 'react'

import * as tailwindVariants from 'tailwind-variants'

import SettingsIcon from 'enso-assets/settings.svg'

import * as textProvider from '#/providers/TextProvider'

import Button from '#/components/styled/Button'
import FocusArea from '#/components/styled/FocusArea'

// =================
// === Constants ===
// =================

const ASSET_INFO_BAR_VARIANTS = tailwindVariants.tv({
  base: 'pointer-events-auto flex h-row shrink-0 cursor-default items-center gap-icons rounded-full bg-frame px-icons-x',
  variants: {
    hidden: { true: 'invisible' },
  },
})

// ====================
// === AssetInfoBar ===
// ====================

/** Props for an {@link AssetInfoBar}. */
export interface AssetInfoBarProps
  extends tailwindVariants.VariantProps<typeof ASSET_INFO_BAR_VARIANTS> {
  /** When `true`, the element occupies space in the layout but is not visible.
   * Defaults to `false`. */
  readonly invisible?: boolean
  readonly isAssetPanelEnabled: boolean
  readonly setIsAssetPanelEnabled: React.Dispatch<React.SetStateAction<boolean>>
}

/** A menubar for displaying asset information. */
export default function AssetInfoBar(props: AssetInfoBarProps) {
  const { invisible = false, isAssetPanelEnabled, setIsAssetPanelEnabled, ...variantProps } = props
  const { getText } = textProvider.useText()

  return (
    <FocusArea active={!invisible} direction="horizontal">
      {innerProps => (
        <div className={ASSET_INFO_BAR_VARIANTS(variantProps)} {...innerProps}>
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
