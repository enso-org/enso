/**
 * @file The React provider (and associated hooks) for determining whether the current focus
 * context is vertical or horizontal.
 */
import type { PropsWithChildren } from 'react'
import { FocusClassesContext } from './constants'
import { useFocusClasses } from './hooks'

/** Props for a {@link FocusClassesProvider}. */
export interface FocusClassesProviderProps extends Readonly<PropsWithChildren> {
  readonly focusChildClass?: string
  readonly focusDefaultClass?: string
}

/**
 * A React provider (and associated hooks) for determining whether the current focus contex
 * is vertical or horizontal.
 */
export function FocusClassesProvider(props: FocusClassesProviderProps) {
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
