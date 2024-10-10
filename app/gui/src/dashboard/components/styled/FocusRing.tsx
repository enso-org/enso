/** @file A styled focus ring. */
import * as React from 'react'

import * as aria from '#/components/aria'

// =================
// === FocusRing ===
// =================

/** Which pseudo-element to place the focus ring on (if any). */
export type FocusRingPlacement = 'after' | 'before' | 'outset'

/** Props for a {@link FocusRing}. */
export interface FocusRingProps extends Readonly<Pick<aria.FocusRingProps, 'children'>> {
  /** Whether to show the focus ring on `:focus-within` instead of `:focus`. */
  readonly within?: boolean
  /**
   * Which pseudo-element to place the focus ring on (if any).
   * Defaults to placement on the actual element.
   */
  readonly placement?: FocusRingPlacement
}

/** A styled focus ring. */
export default function FocusRing(props: FocusRingProps) {
  const { within = false, placement, children } = props
  const focusClass =
    placement === 'outset' ? 'focus-ring-outset'
    : placement === 'before' ? 'before:focus-ring'
    : placement === 'after' ? 'after:focus-ring'
    : 'focus-ring'

  return (
    <aria.FocusRing within={within} focusRingClass={focusClass}>
      {children}
    </aria.FocusRing>
  )
}
