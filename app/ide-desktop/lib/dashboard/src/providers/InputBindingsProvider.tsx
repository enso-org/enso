/** @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context. */
import * as React from 'react'

import * as inputBindingsModule from '#/configurations/inputBindings'

// ============================
// === InputBindingsContext ===
// ============================

/** State contained in a `ShortcutsContext`. */
export interface InputBindingsContextType extends inputBindingsModule.DashboardBindingNamespace {}

const InputBindingsContext = React.createContext<InputBindingsContextType>(
  inputBindingsModule.createBindings(false)
)

/** Props for a {@link InputBindingsProvider}. */
export interface InputBindingsProviderProps extends React.PropsWithChildren<object> {
  readonly inputBindings?: inputBindingsModule.DashboardBindingNamespace
}

// =============================
// === InputBindingsProvider ===
// =============================

/** A React Provider that lets components get the shortcut registry. */
export default function InputBindingsProvider(props: InputBindingsProviderProps) {
  const { inputBindings: inputBindingsRaw, children } = props
  const [inputBindings, setInputBindings] = React.useState(
    () => inputBindingsRaw ?? inputBindingsModule.createBindings(false)
  )

  React.useEffect(() => {
    inputBindings.register()
  }, [inputBindings])

  React.useEffect(() => {
    setInputBindings(oldBindings => {
      oldBindings.unregister()
      return inputBindingsRaw ?? inputBindingsModule.createBindings(false)
    })
  }, [inputBindingsRaw])

  return (
    <InputBindingsContext.Provider value={inputBindings}>{children}</InputBindingsContext.Provider>
  )
}

/** Exposes a property to get the input bindings namespace.
 * @throws {Error} when used outside of its context. */
export function useInputBindings() {
  return React.useContext(InputBindingsContext)
}
