/** @file Hooks for `InputBindings`. */

import { useContext } from 'react'
import { InputBindingsContext } from './constants'

/**
 * Exposes a property to get the input bindings namespace.
 * @throws {Error} when used outside of its context.
 */
export function useInputBindings() {
  return useContext(InputBindingsContext)
}
