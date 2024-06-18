/** @file A styled horizontal menu bar. */
import * as React from 'react'

import FocusArea from '#/components/styled/FocusArea'

// =========================
// === HorizontalMenuBar ===
// =========================

/** Props for a {@link HorizontalMenuBar}. */
export interface HorizontalMenuBarProps extends Readonly<React.PropsWithChildren> {}

/** A styled horizontal menu bar. */
export default function HorizontalMenuBar(props: HorizontalMenuBarProps) {
  const { children } = props

  return (
    <FocusArea direction="horizontal">
      {innerProps => (
        <div className="flex h-row gap-drive-bar" {...innerProps}>
          {children}
        </div>
      )}
    </FocusArea>
  )
}
