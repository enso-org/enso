/** @file A horizontal line dividing two sections in a menu. */
import * as React from 'react'

import * as aria from '#/components/aria'

// =================
// === Separator ===
// =================

/** Props for a {@link Separator}. */
export interface SeparatorProps {
  readonly hidden?: boolean
}

/** A horizontal line dividing two sections in a menu. */
export default function Separator(props: SeparatorProps) {
  const { hidden = false } = props

  return (
    !hidden && (
      <aria.Separator className="mx-context-menu-entry-px my-separator-y border-t-0.5 border-black/[0.16]" />
    )
  )
}
