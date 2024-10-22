/**
 * @file
 *
 * A component visually hides its children from the screen, but keeps them accessible to screen readers.
 */
import * as React from 'react'

import { forwardRef } from '#/utilities/react'
import * as twv from '#/utilities/tailwindVariants'

/** Props for the {@link VisuallyHidden} component. */
export type VisuallyHiddenProps = React.HTMLProps<HTMLElement>

export const VISUALLY_HIDDEN_STYLES = twv.tv({ base: 'sr-only' })

/** A component visually hides its children from the screen, but keeps them accessible to screen readers. */
// eslint-disable-next-line no-restricted-syntax
export const VisuallyHidden = forwardRef(function VisuallyHidden(
  props: VisuallyHiddenProps,
  ref: React.ForwardedRef<HTMLSpanElement>,
) {
  const { className } = props
  return <span ref={ref} className={VISUALLY_HIDDEN_STYLES({ className })} {...props} />
})
