/** @file Hooks for `FocusClassProvider`. */
import { useContext } from 'react'
import { FocusClassesContext } from './constants'

/** The current direction in which focus siblings are located. */
export function useFocusClasses() {
  return useContext(FocusClassesContext)
}
