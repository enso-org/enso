/** @file Constants for `AreaFocusProvider`. */
import { createContext } from 'react'

/** State contained in a `AreaFocusContext`. */
export interface AreaFocusContextType {
  readonly areaFocus: boolean
}

export const AreaFocusContext = createContext<AreaFocusContextType>({ areaFocus: false })
