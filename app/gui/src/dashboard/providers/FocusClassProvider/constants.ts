/** @file Constants for `FocusClassProvider`. */
import { createContext } from 'react'

/** State contained in a `FocusClassesContext`. */
export interface FocusClassesContextType {
  readonly focusChildClass: string
  readonly focusDefaultClass: string
}

export const FocusClassesContext = createContext<FocusClassesContextType>({
  focusChildClass: 'focus-child',
  focusDefaultClass: 'focus-default',
})
