/** @file The React provider for `localStorage`. */
import { LocalStorage } from '#/utilities/LocalStorage'
import { useMemo, type PropsWithChildren } from 'react'
import { LocalStorageContext } from './constants'

/** Props for a {@link LocalStorageProvider}. */
export type LocalStorageProviderProps = Readonly<PropsWithChildren>

/** A React Provider that lets components get the shortcut registry. */
export function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children } = props

  const localStorage = useMemo(() => LocalStorage.getInstance(), [])

  return (
    <LocalStorageContext.Provider value={{ localStorage }}>{children}</LocalStorageContext.Provider>
  )
}
