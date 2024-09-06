/** @file An input that handles focus movement. */
import type { ForwardedRef, RefAttributes } from 'react'

import {
  Input as AriaInput,
  mergeProps,
  type InputProps as AriaInputProps,
} from '#/components/aria'
import { useHandleFocusMove } from '#/hooks/focusHooks'
import { useFocusDirection } from '#/providers/FocusDirectionProvider'
import { forwardRef } from '#/utilities/react'

// =============
// === Input ===
// =============

/** Props for a {@link Input}. */
export interface InputProps extends Readonly<AriaInputProps> {}

export default forwardRef(Input)

/** An input that handles focus movement. */
function Input(props: InputProps, ref: ForwardedRef<HTMLInputElement>) {
  const focusDirection = useFocusDirection()
  const handleFocusMove = useHandleFocusMove(focusDirection)

  return (
    <AriaInput
      {...mergeProps<AriaInputProps & RefAttributes<HTMLInputElement>>()(props, {
        ref,
        className: 'focus-child',
        onKeyDown: handleFocusMove,
      })}
    />
  )
}
