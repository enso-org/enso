/**
 * @file The React provider (and associated hooks) for determining whether the current focus
 * context is vertical or horizontal.
 */
import * as React from 'react'

// ===========================
// === FocusClassesContext ===
// ===========================

/** State contained in a `FocusClassesContext`. */
export interface FocusClassesContextType {
  readonly focusChildClass: string
  readonly focusDefaultClass: string
}

const FocusClassesContext = React.createContext<FocusClassesContextType>({
  focusChildClass: 'focus-child',
  focusDefaultClass: 'focus-default',
})

// ============================
// === FocusClassesProvider ===
// ============================

/** Props for a {@link FocusClassesProvider}. */
export interface FocusClassesProviderProps extends Readonly<React.PropsWithChildren> {
  readonly focusChildClass?: string
  readonly focusDefaultClass?: string
}

/**
 * A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal.
 */
export default function FocusClassesProvider(props: FocusClassesProviderProps) {
  const { focusChildClass: focusChildClassOuter, focusDefaultClass: focusDefaultClassOuter } =
    useFocusClasses()
  const { focusChildClass = focusChildClassOuter } = props
  const { focusDefaultClass = focusDefaultClassOuter, children } = props

  return (
    <FocusClassesContext.Provider value={{ focusChildClass, focusDefaultClass }}>
      {children}
    </FocusClassesContext.Provider>
  )
}

/** The current direction in which focus siblings are located. */
export function useFocusClasses() {
  return React.useContext(FocusClassesContext)
}
