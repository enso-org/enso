/** @file Provider for type-safely storing state in `localStorage`. */
import {
  createContext,
  type PropsWithChildren,
  type SetStateAction,
  useContext,
  useEffect,
  useMemo,
  useState,
} from 'react'

import { z } from 'zod'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { LocalStorage, type LocalStorageKeyMetadata } from '#/utilities/LocalStorage'
import { isFunction, type NonFunction } from '#/utilities/type'

/** State contained in a `LocalStorageContext`. */
export interface LocalStorageContextType {
  readonly localStorage: LocalStorage
}

// @ts-expect-error The default value will never be exposed, as using this without a `Provider`
// is a mistake.
const LocalStorageContext = createContext<LocalStorageContextType>(null)

/** Props for a {@link LocalStorageProvider}. */
export type LocalStorageProviderProps = Readonly<PropsWithChildren>

/** A React Provider that lets components get the shortcut registry. */
export default function LocalStorageProvider(props: LocalStorageProviderProps) {
  const { children } = props

  const localStorage = useMemo(() => LocalStorage.getInstance(), [])

  return (
    <LocalStorageContext.Provider value={{ localStorage }}>{children}</LocalStorageContext.Provider>
  )
}

/** Exposes a property to get the shortcut registry. */
export function useLocalStorage() {
  return useContext(LocalStorageContext)
}

/** Options for {@link defineLocalStorageKey}. */
export interface DefineLocalStorageKeyOptions<Schema extends z.ZodSchema<T>, T = z.infer<Schema>>
  extends Omit<LocalStorageKeyMetadata, 'schema'> {
  readonly schema: (zod: typeof z) => Schema
}

/** Create a set of hooks for interacting with one specific local storage key. */
export function defineLocalStorageKey<Schema extends z.ZodSchema<T>, T = z.infer<Schema>>(
  key: string,
  options: DefineLocalStorageKeyOptions<Schema>,
) {
  /** The type of the value corresponding to this {@link LocalStorage} key. */
  type Value = NonFunction & z.infer<Schema>

  const { schema: makeSchema, ...metadata } = options
  const schema = makeSchema(z)
  LocalStorage.defineKey(key, { ...metadata, schema })

  const validate = (value: unknown): value is T => schema.safeParse(value).success

  const getKey = (localStorage: LocalStorage = LocalStorage.getInstance()) => {
    // eslint-disable-next-line no-restricted-syntax
    return localStorage.get(key) as Value | undefined
  }

  const setKey = (value: Value, localStorage: LocalStorage = LocalStorage.getInstance()) => {
    localStorage.set(key, value)
  }

  const deleteKey = (localStorage: LocalStorage = LocalStorage.getInstance()) => {
    localStorage.delete(key)
  }

  const use = () => {
    const { localStorage } = useLocalStorage()

    const getCallback = useEventCallback(() => getKey(localStorage))
    const setCallback = useEventCallback((value: Value) => {
      setKey(value, localStorage)
    })
    const deleteCallback = useEventCallback(() => {
      deleteKey(localStorage)
    })
    return { get: getCallback, set: setCallback, delete: deleteCallback } as const
  }

  function useLocalStorageState(): readonly [
    value: Value | undefined,
    setValue: (newValue: SetStateAction<Value | undefined>) => void,
    clearValue: () => void,
  ]
  function useLocalStorageState(
    defaultValue: Value,
  ): readonly [
    value: Value,
    setValue: (newValue: SetStateAction<Value>) => void,
    clearValue: () => void,
  ]
  /** Subscribe to Local Storage updates for a specific key. */
  // eslint-disable-next-line no-restricted-syntax
  function useLocalStorageState(
    defaultValue?: Value,
  ): readonly [
    value: Value | undefined,
    setValue: (newValue: SetStateAction<Value | undefined> & SetStateAction<Value>) => void,
    clearValue: () => void,
  ] {
    const { localStorage } = useLocalStorage()

    const [value, privateSetValue] = useState<Value | undefined>(
      () => getKey(localStorage) ?? defaultValue,
    )

    useEffect(
      () =>
        localStorage.subscribe(key, (newValue) => {
          // This is SAFE, assuming the functions in this file are the only functions
          // allowed to set the value.
          // eslint-disable-next-line no-restricted-syntax
          privateSetValue((newValue as Value | undefined) ?? defaultValue)
        }),
      [defaultValue, localStorage],
    )

    const setValue = useEventCallback((newValue: SetStateAction<Value | undefined>) => {
      const nextValue = isFunction(newValue) ? newValue(value) : newValue
      if (nextValue === undefined) {
        deleteKey(localStorage)
      } else {
        setKey(nextValue, localStorage)
      }
    })

    const clearValue = useEventCallback(() => {
      deleteKey(localStorage)
    })

    return [value, setValue, clearValue] as const
  }
  return {
    key: { get: getKey, set: setKey, delete: deleteKey },
    validate,
    use,
    useState: useLocalStorageState,
  }
}
