/**
 * @file
 *
 * A component visually hides its children from the screen, but keeps them accessible to screen readers.
 */
import * as React from 'react'

import * as twv from '#/utilities/tailwindVariants'

/**
 * Props for the {@link VisuallyHidden} component.
 */
export interface VisuallyHiddenProps extends React.HTMLProps<HTMLElement> {}

export const VISUALLY_HIDDEN_STYLES = twv.tv({ base: 'sr-only' })

/**
 * A component visually hides its children from the screen, but keeps them accessible to screen readers.
 */
export const VisuallyHidden = React.forwardRef<HTMLSpanElement, VisuallyHiddenProps>(
  function VisuallyHidden(props, ref) {
    const { className } = props
    return <span ref={ref} className={VISUALLY_HIDDEN_STYLES({ className })} {...props} />
  }
)
