/**
 * @file The React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
import * as React from 'react'

// ========================
// === AreaFocusContext ===
// ========================

/** State contained in a `AreaFocusContext`. */
export interface AreaFocusContextType {
  readonly areaFocus: boolean
}

const AreaFocusContext = React.createContext<AreaFocusContextType>({ areaFocus: false })

/** Props for a {@link AreaFocusProvider}. */
export interface AreaFocusProviderProps
  extends Readonly<React.PropsWithChildren>,
    AreaFocusContextType {}

// =========================
// === AreaFocusProvider ===
// =========================

/**
 * A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
export default function AreaFocusProvider(props: AreaFocusProviderProps) {
  const { areaFocus, children } = props
  return <AreaFocusContext.Provider value={{ areaFocus }}>{children}</AreaFocusContext.Provider>
}

/** Whether the containing area is focused. */
export function useAreaFocus() {
  return React.useContext(AreaFocusContext).areaFocus
}
