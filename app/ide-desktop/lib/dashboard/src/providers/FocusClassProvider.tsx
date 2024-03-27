/** @file The React provider (and associated hooks) for determining whether the current focus
 * context is vertical or horizontal. */
import * as React from 'react'

import * as focusArea from '#/components/styled/FocusArea'

// =========================
// === FocusClassesContext ===
// =========================

/** State contained in a `FocusClassesContext`. */
export interface FocusClassesContextType {
  readonly focusChildClass: string
}

const FocusClassesContext = React.createContext<FocusClassesContextType>({
  focusChildClass: focusArea.DEFAULT_FOCUS_CHILD_CLASS,
})

// ============================
// === FocusClassesProvider ===
// ============================

/** Props for a {@link FocusClassesProvider}. */
export interface FocusClassesProviderProps extends Readonly<React.PropsWithChildren> {
  readonly focusChildClass: string
}

/** A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal. */
export default function FocusClassesProvider(props: FocusClassesProviderProps) {
  const { focusChildClass, children } = props
  return (
    <FocusClassesContext.Provider value={{ focusChildClass }}>
      {children}
    </FocusClassesContext.Provider>
  )
}

/** The current direction in which focus siblings are located. */
export function useFocusClasses() {
  return React.useContext(FocusClassesContext)
}
