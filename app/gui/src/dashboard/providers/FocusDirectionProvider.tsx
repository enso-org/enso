/**
 * @file The React provider (and associated hooks) for determining whether the current focus
 * context is vertical or horizontal.
 */
import * as React from 'react'

// =============================
// === FocusDirectionContext ===
// =============================

/** Possible directions in which focus siblings can be. */
export type FocusDirection = 'horizontal' | 'vertical'

/** State contained in a `FocusDirectionContext`. */
export interface FocusDirectionContextType {
  readonly direction: FocusDirection
}

const FocusDirectionContext = React.createContext<FocusDirectionContextType>({
  direction: 'horizontal',
})

/** Props for a {@link FocusDirectionProvider}. */
export interface FocusDirectionProviderProps
  extends Readonly<React.PropsWithChildren>,
    FocusDirectionContextType {}

// ==============================
// === FocusDirectionProvider ===
// ==============================

/**
 * A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal.
 */
export default function FocusDirectionProvider(props: FocusDirectionProviderProps) {
  const { direction, children } = props
  return (
    <FocusDirectionContext.Provider value={{ direction }}>
      {children}
    </FocusDirectionContext.Provider>
  )
}

/** The current direction in which focus siblings are located. */
export function useFocusDirection() {
  return React.useContext(FocusDirectionContext).direction
}
