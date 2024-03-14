/** @file A styled button. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

/** Props for a {@link Button}. */
export interface ButtonProps {
  readonly focusRing?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When `true`, the button is clickable, but displayed as not clickable.
   * This is mostly useful when letting a button still be keyboard focusable. */
  readonly softDisabled?: boolean
  /** When `true`, the button is not clickable. */
  readonly disabled?: boolean
  readonly image: string
  readonly alt?: string
  /** A title that is only shown when `disabled` is `true`. */
  readonly error?: string | null
  readonly className?: string
  readonly onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
function Button(props: ButtonProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const { focusRing, active = false, softDisabled = false, disabled = false, image, error } = props
  const { alt, className, onClick } = props

  const button = (
    <button
      ref={ref}
      disabled={disabled}
      className={`group flex selectable ${softDisabled ? 'disabled' : ''} ${active ? 'active' : ''}`}
      onClick={onClick}
    >
      <SvgMask
        src={image}
        {...(!active && disabled && error != null ? { title: error } : {})}
        {...(alt != null ? { alt } : {})}
        className={className}
      />
    </button>
  )
  return focusRing == null ? (
    button
  ) : (
    <div
      className={`after:rounded-button-focus-ring after:inset-button-focus-ring-inset relative after:pointer-events-none after:absolute ${focusRing ? 'after:focus-ring' : ''}`}
    >
      {button}
    </div>
  )
}

export default React.forwardRef(Button)
