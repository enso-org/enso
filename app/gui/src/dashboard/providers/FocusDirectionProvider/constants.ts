/** @file Constants for `FocusDirectionProvider`. */
import { createContext } from 'react'

/** Possible directions in which focus siblings can be. */
export type FocusDirection = 'horizontal' | 'vertical'

/** State contained in a `FocusDirectionContext`. */
export interface FocusDirectionContextType {
  readonly direction: FocusDirection
}

export const FocusDirectionContext = createContext<FocusDirectionContextType>({
  direction: 'horizontal',
})
