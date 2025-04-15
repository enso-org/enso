/**
 * @file The React provider for keyboard and mouse shortcuts, along with hooks to use the provider
 * via the shared React context.
 */
import * as React from 'react'

import * as inputBindingsModule from '#/configurations/inputBindings'
import LocalStorage from '#/utilities/LocalStorage'
import * as object from '#/utilities/object'
import { z } from 'zod'
import { useLocalStorage } from './LocalStorageProvider'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly inputBindings: Readonly<Record<string, readonly string[]>>
  }
}

LocalStorage.registerKey('inputBindings', {
  schema: z.record(z.string().array().readonly()).transform((value) =>
    Object.fromEntries(
      Object.entries<unknown>({ ...value }).flatMap((kv) => {
        const [k, v] = kv
        return Array.isArray(v) && v.every((item): item is string => typeof item === 'string') ?
            [[k, v]]
          : []
      }),
    ),
  ),
})

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
  const { children } = props

  const { localStorage } = useLocalStorage()

  const [inputBindings] = React.useState(() => {
    const inputBindingsRaw = inputBindingsModule.createBindings()

    const savedInputBindings = localStorage.get('inputBindings')

    if (savedInputBindings != null) {
      const filteredInputBindings = object.mapEntries(
        inputBindingsRaw.metadata,
        (k) => savedInputBindings[k],
      )
      for (const [bindingKey, newBindings] of object.unsafeEntries(filteredInputBindings)) {
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
      reset: (bindingKey: inputBindingsModule.DashboardBindingKey) => {
        inputBindingsRaw.reset(bindingKey)
        updateLocalStorage()
      },
      add: (bindingKey: inputBindingsModule.DashboardBindingKey, binding: string) => {
        inputBindingsRaw.add(bindingKey, binding)
        updateLocalStorage()
      },
      delete: (bindingKey: inputBindingsModule.DashboardBindingKey, binding: string) => {
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

  React.useEffect(() => {
    inputBindings.register()

    return () => {
      inputBindings.unregister()
    }
  }, [inputBindings])

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
