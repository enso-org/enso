/** @file Hooks for `Checkbox`. */
import { useContext } from 'react'
import { CheckboxContext } from './constants'

/** Gets the context for the checkbox. */
export function useCheckboxContext() {
  return useContext(CheckboxContext)
}

/**
 * Gets the store for the checkbox group.
 * Returns store no matter if the checkbox is inside a group or not.
 * If the checkbox is not inside a group, the `insideGroup` property will be `false`.
 */
export function useCheckboxGroupState() {
  const { store } = useCheckboxContext()
  return store
}
