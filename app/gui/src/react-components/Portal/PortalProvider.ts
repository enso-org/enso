/**
 * @file
 * Provides the context for the Portal component
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

const PortalContext = React.createContext<Element | null>(null)

/** Allows to access the root element for the Portal component */
export function usePortalContext() {
  const root = React.useContext(PortalContext)

  return { root } as const
}

/**
 * Allows to access the root element for the Portal component
 * @throws invariant the `PortalProvider` is not in the component tree
 */
export function useStrictPortalContext() {
  const root = React.useContext(PortalContext)

  invariant(root != null, 'You should use `PortalProvider` to access the `Portal` component')

  return root
}

/** Specifies the root element for the Portal component */
// eslint-disable-next-line no-restricted-syntax
export const PortalProvider = PortalContext.Provider
