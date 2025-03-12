/** @file Hooks for `AreaFocusProvider`. */
import { useContext } from 'react'
import { AreaFocusContext } from './constants'

/** Whether the containing area is focused. */
export function useAreaFocus() {
  return useContext(AreaFocusContext).areaFocus
}
