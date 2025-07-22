/** @file Context for the Portal component. */
import { createContext, useContext } from 'react'

const PortalContext = createContext<Element>(document.body)

/** Allows to access the root element for the Portal component */
export function usePortalContext() {
  return useContext(PortalContext)
}

/** Specifies the root element for the Portal component */
// eslint-disable-next-line no-restricted-syntax
export const PortalProvider = PortalContext.Provider
