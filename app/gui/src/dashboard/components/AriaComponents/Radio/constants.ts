/** @file Constants for `Radio`. */
import { createContext } from 'react'

/** Props for {@link RadioGroupContextProps} */
export interface RadioGroupContextProps {
  /** Whether a Radio element is being pressed (e.g. pointer is currently being held down). */
  readonly pressedRadio: string | null
  /** Sets the pressed Radio element */
  readonly setPressedRadio: (value: string) => void
  /** Clears the pressed Radio element */
  readonly clearPressedRadio: () => void
}

export const RadioGroupContext = createContext<RadioGroupContextProps | null>(null)
