/**
 * @file
 *
 * A component visually hides its children from the screen, but keeps them accessible to screen readers.
 */
import * as React from 'react'

import * as twv from '#/utilities/tailwindVariants'

/** Props for the {@link VisuallyHidden} component. */
export type VisuallyHiddenProps = React.HTMLProps<HTMLElement>

// eslint-disable-next-line react-refresh/only-export-components
export const VISUALLY_HIDDEN_STYLES = twv.tv({ base: 'sr-only' })

/** A component visually hides its children from the screen, but keeps them accessible to screen readers. */
export const VisuallyHidden = React.forwardRef(function VisuallyHidden(
  props: VisuallyHiddenProps,
  ref: React.ForwardedRef<HTMLSpanElement>,
) {
  const { className } = props
  return <span ref={ref} className={VISUALLY_HIDDEN_STYLES({ className })} {...props} />
})
