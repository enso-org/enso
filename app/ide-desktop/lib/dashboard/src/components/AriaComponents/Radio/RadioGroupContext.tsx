/**
 * @file
 *
 * Context provider for `<RadioGroup />` component.
 * Provides useful information about sibling Radio elements within a RadioGroup
 * Allows individual Radio components to communicate with each other via context
 */

import * as React from 'react'

import invariant from 'tiny-invariant'

import * as eventCallback from '#/hooks/eventCallbackHooks'

/**
 * Props for {@link RadioGroupContextProps}
 */
export interface RadioGroupContextProps {
  /**
   * Tells if a Radio element is being pressed
   */
  readonly pressedRadio: string | null
  /**
   * Sets the pressed Radio element
   */
  readonly setPressedRadio: (value: string) => void
  /**
   * Clears the pressed Radio element
   */
  readonly clearPressedRadio: () => void
}

const RadioGroupContext = React.createContext<RadioGroupContextProps | null>(null)

/**
 * RadioGroupProvider is a context provider for RadioGroup component
 * Allows individual Radio components to communicate with each other
 */
export function RadioGroupProvider(props: React.PropsWithChildren) {
  const { children } = props

  const [pressedRadio, setPressedRadio] = React.useState<string | null>(null)
  const setRadioPressed = eventCallback.useEventCallback((value: string) => {
    setPressedRadio(value)
  })

  const clearPressedRadio = eventCallback.useEventCallback(() => {
    setPressedRadio(null)
  })

  const value = React.useMemo<RadioGroupContextProps>(
    () => ({
      pressedRadio,
      setPressedRadio: setRadioPressed,
      clearPressedRadio,
    }),
    [pressedRadio, setRadioPressed, clearPressedRadio]
  )

  return <RadioGroupContext.Provider value={value}>{children}</RadioGroupContext.Provider>
}

/**
 * Props for {@link useRadioGroupContext}
 */
export interface UseRadioGroupContextProps {
  readonly value: string
}

/**
 * Provides useful information about sibling Radio elements within a RadioGroup
 */
export function useRadioGroupContext(props: UseRadioGroupContextProps) {
  const { value } = props
  const context = React.useContext(RadioGroupContext)

  invariant(context != null, 'You can only use radio inside RadioGroup')

  const isSiblingPressed = context.pressedRadio != null && value !== context.pressedRadio

  const setPressed = eventCallback.useEventCallback(() => {
    context.setPressedRadio(value)
  })

  const clearPressed = eventCallback.useEventCallback(() => {
    context.clearPressedRadio()
  })

  return {
    isSiblingPressed,
    setPressed,
    clearPressed,
  }
}
