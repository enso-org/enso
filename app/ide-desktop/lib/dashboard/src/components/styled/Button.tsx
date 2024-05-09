/** @file A styled button. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

// ==============
// === Button ===
// ==============

/** Props for a {@link Button}. */
export interface ButtonProps {
  readonly autoFocus?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly active?: boolean
  /** When `true`, the button is clickable, but displayed as not clickable.
   * This is mostly useful when letting a button still be keyboard focusable. */
  readonly softDisabled?: boolean
  /** When `true`, the button is not clickable. */
  readonly isDisabled?: boolean
  readonly image: string
  readonly alt?: string
  /** A title that is only shown when `disabled` is `true`. */
  readonly error?: string | null
  readonly className?: string
  readonly onPress: (event: aria.PressEvent) => void
}

/** A styled button. */
function Button(props: ButtonProps, ref: React.ForwardedRef<HTMLButtonElement>) {
  const {
    active = false,
    softDisabled = false,
    image,
    error,
    alt,
    className,
    ...buttonProps
  } = props
  const { isDisabled = false } = buttonProps
  const focusChildProps = focusHooks.useFocusChild()

  return (
    <FocusRing placement="after">
      <aria.Button
        {...aria.mergeProps<aria.ButtonProps>()(buttonProps, focusChildProps, {
          ref,
          className:
            'relative after:pointer-events-none after:absolute after:inset after:rounded-button-focus-ring transition-colors hover:enabled:bg-primary/10 rounded-button-focus-ring m-button-focus-ring-inset p-negative-button-focus-ring-inset',
        })}
      >
        <div
          className={`group flex selectable ${isDisabled || softDisabled ? 'disabled' : ''} ${active ? 'active' : ''}`}
        >
          <SvgMask
            src={image}
            {...(!active && isDisabled && error != null ? { title: error } : {})}
            {...(alt != null ? { alt } : {})}
            className={className}
          />
        </div>
      </aria.Button>
    </FocusRing>
  )
}

export default React.forwardRef(Button)
