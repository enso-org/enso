/**
 * @file
 *
 * Context for the Text component.
 */
import * as React from 'react'

/** Context for the Text component. */
export interface TextContextType {
  /** Flag indicating whether the component is inside a Text component. */
  readonly isInsideTextComponent: boolean
}

const TextContext = React.createContext<TextContextType>({
  isInsideTextComponent: false,
})

/** Hook to get the Text context. */
// eslint-disable-next-line react-refresh/only-export-components
export function useTextContext(): TextContextType {
  return React.useContext(TextContext)
}

// eslint-disable-next-line no-restricted-syntax
export const TextProvider = TextContext.Provider
