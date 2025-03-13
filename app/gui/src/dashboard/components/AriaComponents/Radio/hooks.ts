/** @file Hooks for `Radio`. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { RadioGroupContext } from './constants'

/** Props for {@link useRadioGroupContext} */
export interface UseRadioGroupContextOptions {
  readonly value: string
}

/** Provides useful information about sibling Radio elements within a RadioGroup */
export function useRadioGroupContext(props: UseRadioGroupContextOptions) {
  const { value } = props
  const context = useContext(RadioGroupContext)

  invariant(context != null, 'You can only use radio inside RadioGroup')

  /**
   * Tells if a sibling Radio element is being pressed
   * It's not the same as selected value, instead it says if a user is clicking on a sibling Radio element now.
   */
  const isSiblingPressed = context.pressedRadio != null && value !== context.pressedRadio

  const setPressed = useEventCallback(() => {
    context.setPressedRadio(value)
  })

  const clearPressed = useEventCallback(() => {
    context.clearPressedRadio()
  })

  return {
    isSiblingPressed,
    setPressed,
    clearPressed,
  }
}
