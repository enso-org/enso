/**
 * @file The React provider (and associated hooks) for determining whether the current focus
 * context is vertical or horizontal.
 */
import {
  FocusDirectionContext,
  type FocusDirectionContextType,
} from '#/providers/FocusDirectionProvider/constants'
import type { PropsWithChildren } from 'react'

/** Props for a {@link FocusDirectionProvider}. */
export interface FocusDirectionProviderProps
  extends Readonly<PropsWithChildren>,
    FocusDirectionContextType {}

/**
 * A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal.
 */
export function FocusDirectionProvider(props: FocusDirectionProviderProps) {
  const { direction, children } = props
  return (
    <FocusDirectionContext.Provider value={{ direction }}>
      {children}
    </FocusDirectionContext.Provider>
  )
}
