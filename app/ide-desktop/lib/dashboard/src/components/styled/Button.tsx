/** @file A styled button. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
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
  readonly buttonClassName?: string
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
    buttonClassName,
    className,
    ...buttonProps
  } = props
  const { isDisabled = false } = buttonProps
  const focusChildProps = focusHooks.useFocusChild()

  const button = (
    <FocusRing placement="after">
      <aria.Button
        {...aria.mergeProps<aria.ButtonProps>()(buttonProps, focusChildProps, {
          ref,
          className:
            buttonClassName ??
            'relative after:pointer-events-none after:absolute after:inset-button-focus-ring-inset after:rounded-button-focus-ring',
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

  return alt == null ? (
    button
  ) : (
    <ariaComponents.TooltipTrigger>
      {button}
      <ariaComponents.Tooltip>{alt}</ariaComponents.Tooltip>
    </ariaComponents.TooltipTrigger>
  )
}

export default React.forwardRef(Button)
