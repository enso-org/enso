/**
 * @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context.
 */
import * as React from 'react'

import * as inputBindingsModule from '#/configurations/inputBindings'

/** State contained in a `ShortcutsContext`. */
export type InputBindingsContextType = inputBindingsModule.DashboardBindingNamespace

const InputBindingsContext = React.createContext<InputBindingsContextType>(
  inputBindingsModule.createBindings(),
)

/** Props for a {@link InputBindingsProvider}. */
export interface InputBindingsProviderProps extends Readonly<React.PropsWithChildren> {
  readonly inputBindings?: inputBindingsModule.DashboardBindingNamespace
}

/** A React Provider that lets components get the input bindings. */
export default function InputBindingsProvider(props: InputBindingsProviderProps) {
  const { inputBindings: inputBindingsRaw, children } = props
  const [inputBindings, setInputBindings] = React.useState(
    () => inputBindingsRaw ?? inputBindingsModule.createBindings(),
  )

  React.useEffect(() => {
    inputBindings.register()
    return () => {
      inputBindings.unregister()
    }
  }, [inputBindings])

  React.useEffect(() => {
    setInputBindings(inputBindingsRaw ?? inputBindingsModule.createBindings())
  }, [inputBindingsRaw])

  return (
    <InputBindingsContext.Provider value={inputBindings}>{children}</InputBindingsContext.Provider>
  )
}

/**
 * Exposes a property to get the input bindings namespace.
 * @throws {Error} when used outside of its context.
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useInputBindings() {
  return React.useContext(InputBindingsContext)
}
