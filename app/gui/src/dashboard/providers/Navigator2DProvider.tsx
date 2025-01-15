/**
 * @file The React provider for 2D navigation, along with hooks to use the provider via
 * the shared React context.
 */
import * as React from 'react'

import Navigator2D from '#/utilities/Navigator2D'

// ==========================
// === Navigator2DContext ===
// ==========================

/** State contained in a `ShortcutsContext`. */
export type Navigator2DContextType = Navigator2D

const Navigator2DContext = React.createContext<Navigator2DContextType>(new Navigator2D())

/** Props for a {@link Navigator2DProvider}. */
export type Navigator2DProviderProps = Readonly<React.PropsWithChildren>

// ===========================
// === Navigator2DProvider ===
// ===========================

/** A React Provider that lets components get the 2D navigator. */
export default function Navigator2DProvider(props: Navigator2DProviderProps) {
  const { children } = props
  const [navigator2D] = React.useState(() => new Navigator2D())

  return <Navigator2DContext.Provider value={navigator2D}>{children}</Navigator2DContext.Provider>
}

/** Exposes a property to get the 2D navigator namespace. */
export function useNavigator2D() {
  return React.useContext(Navigator2DContext)
}
