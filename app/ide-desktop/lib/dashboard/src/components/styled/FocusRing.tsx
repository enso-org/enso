/** @file A styled focus ring. */
import * as React from 'react'

import * as aria from '#/components/aria'

/** Props for a {@link FocusRing}. */
export interface FocusRingProps extends Readonly<Pick<aria.FocusRingProps, 'children'>> {
  /** Which pseudo-element to place the focus ring on (if any).
   * Defaults to placement on the actual element. */
  readonly placement?: 'after' | 'before'
}

/** A styled focus ring. */
export default function FocusRing(props: FocusRingProps) {
  const { placement, children } = props
  const focusClass =
    placement === 'before'
      ? 'before:focus-ring'
      : placement === 'after'
        ? 'after:focus-ring'
        : 'focus-ring'

  return <aria.FocusRing focusClass={focusClass}>{children}</aria.FocusRing>
}
