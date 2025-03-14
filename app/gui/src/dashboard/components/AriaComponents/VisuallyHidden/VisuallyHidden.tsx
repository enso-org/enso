/** @file Visually hide its children from the screen, but keeps them accessible to screen readers. */
import { forwardRef } from '#/utilities/react'
import type { ForwardedRef, HTMLProps } from 'react'
import { VISUALLY_HIDDEN_STYLES } from './variants'

/** Props for the {@link VisuallyHidden} component. */
export type VisuallyHiddenProps = HTMLProps<HTMLElement>

/** A component visually hides its children from the screen, but keeps them accessible to screen readers. */
export const VisuallyHidden = forwardRef(function VisuallyHidden(
  props: VisuallyHiddenProps,
  ref: ForwardedRef<HTMLSpanElement>,
) {
  const { className } = props

  return <span ref={ref} className={VISUALLY_HIDDEN_STYLES({ className })} {...props} />
})
