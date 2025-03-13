/**
 * @file
 *
 * Context provider for `<RadioGroup />` component.
 * Provides useful information about sibling Radio elements within a RadioGroup
 * Allows individual Radio components to communicate with each other via context
 *
 * This component is not related to `RadioGroupStateContext` from `react-aria-components`,
 * which is used to manage the state of a radio group (selected value, disabled, etc.)
 *
 * This component is supposed to provide custom context information for Radio components
 * and let them communicate with each other (e.g. to know if a sibling Radio element is being pressed)
 */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMemo, useState, type PropsWithChildren } from 'react'
import { RadioGroupContext, type RadioGroupContextProps } from './constants'

/**
 * Context provider for RadioGroup component
 * Allows individual Radio components to communicate with each other.
 */
export function RadioGroupProvider(props: PropsWithChildren) {
  const { children } = props

  const [pressedRadio, setPressedRadio] = useState<string | null>(null)
  const setRadioPressed = useEventCallback((value: string) => {
    setPressedRadio(value)
  })

  const clearPressedRadio = useEventCallback(() => {
    setPressedRadio(null)
  })

  const value = useMemo<RadioGroupContextProps>(
    () => ({
      pressedRadio,
      setPressedRadio: setRadioPressed,
      clearPressedRadio,
    }),
    [pressedRadio, setRadioPressed, clearPressedRadio],
  )

  return <RadioGroupContext.Provider value={value}>{children}</RadioGroupContext.Provider>
}
