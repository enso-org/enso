/**
 * @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context.
 */
import {
  createBindings,
  type DashboardBindingKey,
  type DashboardBindingNamespace,
} from '#/configurations/inputBindings'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { mapEntries, unsafeEntries } from '#/utilities/object'
import { useEffect, useState, type PropsWithChildren } from 'react'
import { InputBindingsContext } from './constants'

/** Props for a {@link InputBindingsProvider}. */
export interface InputBindingsProviderProps extends Readonly<PropsWithChildren> {
  readonly inputBindings?: DashboardBindingNamespace
}

/** A React Provider that lets components get the input bindings. */
export function InputBindingsProvider(props: InputBindingsProviderProps) {
  const { children } = props

  const { localStorage } = useLocalStorage()

  const [inputBindings] = useState(() => {
    const inputBindingsRaw = createBindings()

    const savedInputBindings = localStorage.get('inputBindings')

    if (savedInputBindings != null) {
      const filteredInputBindings = mapEntries(
        inputBindingsRaw.metadata,
        (k) => savedInputBindings[k],
      )
      for (const [bindingKey, newBindings] of unsafeEntries(filteredInputBindings)) {
        for (const oldBinding of inputBindingsRaw.metadata[bindingKey].bindings) {
          inputBindingsRaw.delete(bindingKey, oldBinding)
        }
        for (const newBinding of newBindings ?? []) {
          inputBindingsRaw.add(bindingKey, newBinding)
        }
      }
    }

    const updateLocalStorage = () => {
      localStorage.set(
        'inputBindings',
        Object.fromEntries(
          Object.entries(inputBindingsRaw.metadata).map((kv) => {
            const [k, v] = kv
            return [k, v.bindings]
          }),
        ),
      )
    }
    return {
      /** Transparently pass through `handler()`. */
      get handler() {
        return inputBindingsRaw.handler.bind(inputBindingsRaw)
      },
      /** Transparently pass through `attach()`. */
      get attach() {
        return inputBindingsRaw.attach.bind(inputBindingsRaw)
      },
      reset: (bindingKey: DashboardBindingKey) => {
        inputBindingsRaw.reset(bindingKey)
        updateLocalStorage()
      },
      add: (bindingKey: DashboardBindingKey, binding: string) => {
        inputBindingsRaw.add(bindingKey, binding)
        updateLocalStorage()
      },
      delete: (bindingKey: DashboardBindingKey, binding: string) => {
        inputBindingsRaw.delete(bindingKey, binding)
        updateLocalStorage()
      },
      /** Transparently pass through `metadata`. */
      get metadata() {
        return inputBindingsRaw.metadata
      },
      /** Transparently pass through `register()`. */
      get register() {
        return inputBindingsRaw.unregister.bind(inputBindingsRaw)
      },
      /** Transparently pass through `unregister()`. */
      get unregister() {
        return inputBindingsRaw.unregister.bind(inputBindingsRaw)
      },
    }
  })

  useEffect(() => {
    inputBindings.register()

    return () => {
      inputBindings.unregister()
    }
  }, [inputBindings])

  return (
    <InputBindingsContext.Provider value={inputBindings}>{children}</InputBindingsContext.Provider>
  )
}
