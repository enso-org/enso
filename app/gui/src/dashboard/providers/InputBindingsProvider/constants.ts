/** @file Constants for `InputBindings`. */
import { createBindings, type DashboardBindingNamespace } from '#/configurations/inputBindings'
import { createContext } from 'react'

/** State contained in a `ShortcutsContext`. */
export type InputBindingsContextType = DashboardBindingNamespace

export const InputBindingsContext = createContext<InputBindingsContextType>(createBindings())
