/** @file An input that handles focus movement. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'
import { forwardRef } from '#/utilities/react'

// =============
// === Input ===
// =============

/** Props for a {@link Input}. */
export interface InputProps extends Readonly<aria.InputProps> {}

export default forwardRef(Input)

/** An input that handles focus movement. */
function Input(props: InputProps, ref: React.ForwardedRef<HTMLInputElement>) {
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <aria.Input
      {...aria.mergeProps<aria.InputProps & React.RefAttributes<HTMLInputElement>>()(props, {
        ref,
        className: 'focus-child',
        onKeyDown: handleFocusMove,
      })}
    />
  )
}
