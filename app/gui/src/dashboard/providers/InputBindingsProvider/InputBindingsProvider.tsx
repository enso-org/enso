/**
 * @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context.
 */
import { createBindings, type DashboardBindingNamespace } from '#/configurations/inputBindings'
import { useEffect, useState, type PropsWithChildren } from 'react'
import { InputBindingsContext } from './constants'

/** Props for a {@link InputBindingsProvider}. */
export interface InputBindingsProviderProps extends Readonly<PropsWithChildren> {
  readonly inputBindings?: DashboardBindingNamespace
}

/** A React Provider that lets components get the input bindings. */
export function InputBindingsProvider(props: InputBindingsProviderProps) {
  const { inputBindings: inputBindingsRaw, children } = props
  const [inputBindings, setInputBindings] = useState(() => inputBindingsRaw ?? createBindings())

  useEffect(() => {
    inputBindings.register()
    return () => {
      inputBindings.unregister()
    }
  }, [inputBindings])

  useEffect(() => {
    setInputBindings(inputBindingsRaw ?? createBindings())
  }, [inputBindingsRaw])

  return (
    <InputBindingsContext.Provider value={inputBindings}>{children}</InputBindingsContext.Provider>
  )
}
