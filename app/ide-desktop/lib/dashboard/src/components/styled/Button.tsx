/** @file A styled button. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

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
export default function Button(props: ButtonProps) {
  const { autoFocus = false, active = false, softDisabled = false, isDisabled = false } = props
  const { image, error, alt, className, onPress } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <FocusRing placement="after">
      <aria.Button
        autoFocus={autoFocus}
        isDisabled={isDisabled}
        className="focus-child relative after:pointer-events-none after:absolute after:inset-button-focus-ring-inset after:rounded-button-focus-ring"
        onPress={onPress}
        onKeyDown={handleFocusMove}
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
