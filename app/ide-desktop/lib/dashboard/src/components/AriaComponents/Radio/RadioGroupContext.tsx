/**
 * @file
 *
 * This file provides a context for RadioGroup and Radio components.
 */

import * as React from 'react'

import invariant from 'tiny-invariant'

import * as eventCallback from '#/hooks/eventCallbackHooks'

/**
 * Props for {@link RadioGroupContextProps}
 */
export interface RadioGroupContextProps {
  readonly pressedRadio: string | null
  readonly setRadioPressed: (value: string) => void
  readonly clearPressedRadio: () => void
}

const RadioGroupContext = React.createContext<RadioGroupContextProps | null>(null)

/**
 * RadioGroupProvider is a context provider for RadioGroup
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
      setRadioPressed,
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
 * Provides useful information about subling Radio elements within a RadioGroup
 */
export function useRadioGroupContext(props: UseRadioGroupContextProps) {
  const { value } = props
  const ctx = React.useContext(RadioGroupContext)

  invariant(ctx != null, 'You can only use radio inside RadioGroup')

  const isSiblingPressed = ctx.pressedRadio != null && value !== ctx.pressedRadio

  const setPressed = eventCallback.useEventCallback(() => {
    ctx.setRadioPressed(value)
  })

  const clearPressed = eventCallback.useEventCallback(() => {
    ctx.clearPressedRadio()
  })

  return {
    isSiblingPressed,
    setPressed,
    clearPressed,
  }
}
