/**
 * @file The React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
import type { PropsWithChildren } from 'react'
import { AreaFocusContext, type AreaFocusContextType } from './constants'

/** Props for a {@link AreaFocusProvider}. */
export interface AreaFocusProviderProps extends Readonly<PropsWithChildren>, AreaFocusContextType {}

/**
 * A React provider (and associated hooks) for determining whether the current area
 * containing the current element is focused.
 */
export function AreaFocusProvider(props: AreaFocusProviderProps) {
  const { areaFocus, children } = props
  return <AreaFocusContext.Provider value={{ areaFocus }}>{children}</AreaFocusContext.Provider>
}
