/** @file A styled horizontal menu bar. */
import * as React from 'react'

import FocusArea from '#/components/styled/FocusArea'

import * as tailwindVariants from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const HORIZONTAL_MENU_BAR_VARIANTS = tailwindVariants.tv({
  base: 'flex items-center h-row gap-drive-bar',
  variants: {
    grow: { true: 'grow' },
  },
})

// =========================
// === HorizontalMenuBar ===
// =========================

/** Props for a {@link HorizontalMenuBar}. */
export interface HorizontalMenuBarProps
  extends Readonly<React.PropsWithChildren>,
    Readonly<tailwindVariants.VariantProps<typeof HORIZONTAL_MENU_BAR_VARIANTS>> {
  readonly className?: string
}

/** A styled horizontal menu bar. */
export default function HorizontalMenuBar(props: HorizontalMenuBarProps) {
  const { children, ...variantProps } = props

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div className={HORIZONTAL_MENU_BAR_VARIANTS(variantProps)} {...innerProps}>
          {children}
        </div>
      )}
    </FocusArea>
  )
}
