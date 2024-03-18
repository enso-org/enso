import { createContext, useContext, type RefObject } from 'react'

import invariant from 'tiny-invariant'

const PortalContext = createContext<RefObject<Element | null> | null>(null)

/**
 * Allows to access the root element for the Portal component
 */
export function usePortalContext() {
  const root = useContext(PortalContext)

  return { root } as const
}

/**
 * Allows to access the root element for the Portal component
 * @throws invariant the `PortalProvider` is not in the component tree
 */
export function useStrictPortalContext() {
  const root = useContext(PortalContext)

  invariant(
    root != null && root.current != null,
    'You should use `PortalProvider` to access the `Portal` component'
  )

  // this is safe because we are using `invariant` to check if the `PortalProvider` is in the component tree
  // eslint-disable-next-line no-restricted-syntax
  return root as { current: Element }
}

/**
 * Specifies the root element for the Portal component
 */
export const PortalProvider = PortalContext.Provider
