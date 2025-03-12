/** @file Constants for `LocalStorageProvider`. */
import type { LocalStorage } from '#/utilities/LocalStorage'
import { createContext } from 'react'

/** State contained in a `LocalStorageContext`. */
export interface LocalStorageContextType {
  readonly localStorage: LocalStorage
}

export const LocalStorageContext = createContext<LocalStorageContextType | null>(null)
