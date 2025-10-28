/** @file Provider for focus scope for input bindings. */
import { createContext, useContext } from 'react'

export const BindingFocusScopeContext = createContext<React.RefObject<HTMLElement>>({
  current: document.body,
})

/** Get the binding focus scope from its context. */
export function useBindingFocusScope() {
  return useContext(BindingFocusScopeContext)
}
