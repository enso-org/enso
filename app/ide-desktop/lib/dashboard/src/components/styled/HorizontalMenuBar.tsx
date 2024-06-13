/** @file A styled horizontal menu bar. */
import * as React from 'react'

import * as tailwindVariants from 'tailwind-variants'

import FocusArea from '#/components/styled/FocusArea'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const HORIZONTAL_MENU_BAR_VARIANTS = tailwindVariants.tv({
  base: 'flex h-row gap-drive-bar',
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
  const { children, className, ...variantProps } = props

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div
          className={tailwindMerge.twMerge(HORIZONTAL_MENU_BAR_VARIANTS(variantProps), className)}
          {...innerProps}
        >
          {children}
        </div>
      )}
    </FocusArea>
  )
}
