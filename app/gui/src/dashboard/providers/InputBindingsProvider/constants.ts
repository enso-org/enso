/** @file Constants for `InputBindings`. */
import { createBindings, type DashboardBindingNamespace } from '#/configurations/inputBindings'
import { LocalStorage } from '#/utilities/LocalStorage'
import { createContext } from 'react'
import { z } from 'zod'

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
export type InputBindingsContextType = DashboardBindingNamespace

export const InputBindingsContext = createContext<InputBindingsContextType>(createBindings())
