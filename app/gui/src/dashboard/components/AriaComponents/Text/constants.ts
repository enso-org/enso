/** @file Constants for `Text`. */
import { createContext } from 'react'

/** Context for the Text component. */
export interface TextContextType {
  /** Flag indicating whether the component is inside a Text component. */
  readonly isInsideTextComponent: boolean
}

export const TextContext = createContext<TextContextType>({
  isInsideTextComponent: false,
})
