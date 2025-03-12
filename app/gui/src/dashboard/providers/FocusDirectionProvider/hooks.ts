/** @file Hooks for `FocusDirectionProvider`. */
import { FocusDirectionContext } from '#/providers/FocusDirectionProvider/constants'
import { useContext } from 'react'

/** The current direction in which focus siblings are located. */
export function useFocusDirection() {
  return useContext(FocusDirectionContext).direction
}
