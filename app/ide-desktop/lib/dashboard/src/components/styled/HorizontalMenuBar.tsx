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
      {(ref, innerProps) => (
        <div ref={ref} className="flex gap-drive-bar" {...innerProps}>
          {children}
        </div>
      )}
    </FocusArea>
  )
}
