/** @file A LocalStorage data manager. */
import { useVueValue } from '$/providers/react/common'
import * as common from 'enso-common/src/constants'
import * as object from 'enso-common/src/utilities/data/object'
import { IS_DEV_MODE } from 'enso-common/src/utilities/detect'
import { useCallback } from 'react'
import invariant from 'tiny-invariant'
import { shallowReactive, toRaw } from 'vue'
import * as z from 'zod'

const KEY_DEFINITION_STACK_TRACES = new Map<string, string>()

/**
 * Whether the source location for `LocalStorage.register(key)` is different to the previous
 * known source location.
 */
function isSourceChanged(key: string) {
  const stack = (new Error().stack ?? '').replace(/[?]t=\d+:\d+:\d+/g, '')
  const isChanged = stack !== KEY_DEFINITION_STACK_TRACES.get(key)
  KEY_DEFINITION_STACK_TRACES.set(key, stack)
  return isChanged
}

/** Metadata describing runtime behavior associated with a {@link LocalStorageKey}. */
export interface LocalStorageKeyMetadata<K extends LocalStorageKey> {
  readonly isUserSpecific?: boolean
  /**
   * The Zod schema to validate the value.
   * If this is provided, the value will be parsed using this schema.
   * If this is not provided, the value will be parsed using the `tryParse` function.
   */
  readonly schema: z.ZodType<LocalStorageData[K]>
}

/**
 * The data that can be stored in a {@link LocalStorage}.
 * Declaration merge into this interface to add a new key.
 */
export interface LocalStorageData {
  // Add a dummy key to avoid type errors for configurations that don't import
  // any files that merge declarations into `LocalStorageData`.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly _dummyLocalStorageKey: true
}

/** All possible keys of a {@link LocalStorage}. */
export type LocalStorageKey = keyof LocalStorageData

/** Metadata for each storage key. */
type KeyMetadata = {
  [K in LocalStorageKey]: LocalStorageKeyMetadata<K>
}

/** Compute the actual storage key from `LocalStorage` entry key. */
function getItemKey(key: LocalStorageKey) {
  return `${common.PRODUCT_NAME}::${key}`
}

/** A LocalStorage data manager. */
export default class LocalStorage {
  private static readonly keyMetadata: Partial<KeyMetadata> = {}
  private static instance: LocalStorage | null = null
  private values: Partial<LocalStorageData>
  private readonly eventTarget = new EventTarget()

  /** Create a {@link LocalStorage}. */
  private constructor() {
    this.values = shallowReactive<Partial<LocalStorageData>>({})
  }

  /**
   * Gets the singleton instance of {@link LocalStorage}.
   */
  static getInstance() {
    if (LocalStorage.instance == null) {
      LocalStorage.instance = new LocalStorage()
    }

    return LocalStorage.instance
  }

  /** Get all {@link LocalStorageKey} variants that has been registered using {@link registerKey} method so far. */
  static getAllRegisteredKeys(): LocalStorageKey[] {
    // eslint-disable-next-line no-restricted-syntax
    return Object.keys(LocalStorage.keyMetadata) as LocalStorageKey[]
  }

  /** Register runtime behavior associated with a {@link LocalStorageKey}. */
  static registerKey<K extends LocalStorageKey>(key: K, metadata: LocalStorageKeyMetadata<K>) {
    if (IS_DEV_MODE ? isSourceChanged(key) : true) {
      invariant(
        !(key in LocalStorage.keyMetadata),
        `Local storage key '${key}' has already been registered.`,
      )
    }
    Object.assign(LocalStorage.keyMetadata, { [key]: metadata })
  }

  /**
   * Get the metadata for a key.
   */
  static getKeyMetadata<K extends LocalStorageKey>(key: K): LocalStorageKeyMetadata<K> {
    const meta = LocalStorage.keyMetadata[key]
    invariant(meta != null, `Local storage key '${key}' is not yet registered.`)
    return meta
  }

  /** Retrieve an entry from the stored data. */
  get<K extends LocalStorageKey>(key: K) {
    this.assertRegisteredKey(key)

    if (!(key in this.values)) {
      const value = this.readValueFromLocalStorage(key)
      if (value != null) this.values[key] = value
    }

    return this.values[key]
  }

  /** Write an entry to the stored data, and save. */
  set<K extends LocalStorageKey>(key: K, value: LocalStorageData[K]) {
    this.assertRegisteredKey(key)

    this.values[key] = value

    this.save(key)
    this.eventTarget.dispatchEvent(new Event(key))
  }

  /**
   * Set an entry in the stored data from an untrusted source.
   * This is useful for setting the state from a clipboard or from a remote source/user input.
   */
  setFromUntrustedSource(key: unknown, value: unknown) {
    const schema = z
      .object({
        key: z
          .custom<LocalStorageKey>()
          .refine((unknownKey) => typeof unknownKey === 'string')
          .refine((unknownKey) => unknownKey in LocalStorage.keyMetadata),
        value: z.any(),
      })
      .transform((data) => {
        const valueSchema = LocalStorage.getKeyMetadata(data.key).schema
        const parsedValue = valueSchema.safeParse(data.value)

        if (parsedValue.success) {
          return { key: data.key, value: parsedValue.data }
        }

        throw new Error('Invalid key or value')
      })

    const parsed = schema.safeParse({ key, value })

    if (parsed.success) {
      this.set(parsed.data.key, parsed.data.value)
    }
  }

  /**
   * Set all entries in the stored data from an untrusted source.
   * This is useful for setting the state from a clipboard or from a remote source/user input.
   */
  setManyFromUntrustedSource(values: unknown) {
    if (typeof values !== 'object' || values == null) {
      return
    }

    const keys = LocalStorage.getAllRegisteredKeys()

    for (const key of keys) {
      // This is safe because we asserted that `values` is an object.
      // eslint-disable-next-line no-restricted-syntax
      const value: unknown = key in values ? (values as Record<string, unknown>)[key] : null
      this.setFromUntrustedSource(key, value)
    }
  }

  /**
   * Delete an entry from the stored data, and save.
   */
  delete<K extends LocalStorageKey>(key: K) {
    this.assertRegisteredKey(key)
    // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
    delete this.values[key]
    this.save(key)
    this.eventTarget.dispatchEvent(new Event(key))
  }

  /**
   * Read a value from the stored data, and delete it after reading.
   */
  consume<K extends LocalStorageKey>(key: K) {
    const value = this.get(key)
    this.delete(key)
    return value
  }

  /** Delete user-specific entries from the stored data, and save. */
  clearUserSpecificEntries() {
    for (const [key, metadata] of object.unsafeEntries(LocalStorage.keyMetadata)) {
      if (metadata?.isUserSpecific === true) {
        this.delete(key)
      }
    }
  }

  /** Delete all entries from the stored data. */
  clearAll() {
    LocalStorage.getAllRegisteredKeys().forEach((key) => this.delete(key))
  }

  /** Add an event listener to a specific key. */
  subscribe<K extends LocalStorageKey>(
    key: K,
    callback: (value: LocalStorageData[K] | undefined) => void,
  ) {
    const onChange = () => {
      callback(this.get(key))
    }
    this.eventTarget.addEventListener(key, onChange)

    return () => {
      this.eventTarget.removeEventListener(key, onChange)
    }
  }

  /** Save the current value of the stored data.. */
  protected save(key: LocalStorageKey) {
    // Make values raw, so any watchEffect setting values will not be triggered unnecessarily.
    const rawValues = toRaw(this.values)
    const valueToSave = rawValues[key]
    const itemKey = getItemKey(key)
    try {
      if (valueToSave == null) localStorage.removeItem(itemKey)
      else localStorage.setItem(itemKey, JSON.stringify(valueToSave))
    } catch (error) {
      // eslint-disable-next-line no-restricted-properties
      console.warn('LocalStorage failed to persist data', { key, error })
    }
  }

  /**
   * Whether the key has been registered.
   * @throws {Error} If the key has not been registered yet.
   */
  private assertRegisteredKey(key: LocalStorageKey): asserts key is LocalStorageKey {
    if (key in LocalStorage.keyMetadata) {
      return
    }

    throw new Error(
      `Local storage key '${key}' has not been registered yet. Please register it first.`,
    )
  }

  /** Read a value from the stored data. */
  private readValueFromLocalStorage<Key extends LocalStorageKey>(
    key: Key,
  ): LocalStorageData[Key] | null {
    this.assertRegisteredKey(key)

    const storedJson = localStorage.getItem(getItemKey(key))
    let storedValue: unknown

    try {
      storedValue = storedJson != null ? JSON.parse(storedJson) : null
    } catch (error) {
      // eslint-disable-next-line no-restricted-properties
      console.warn('LocalStorage failed to parse JSON', { key, error })
    }

    if (storedValue == null) return null

    const valueSchema = LocalStorage.getKeyMetadata(key).schema
    const parsedValue = valueSchema.safeParse(storedValue)
    if (parsedValue.success) return parsedValue.data

    const error = parsedValue.error
    // eslint-disable-next-line no-restricted-properties
    console.warn('LocalStorage failed to parse value', { key, storedValue, error })

    return null
  }
}

/** React hook for viewing whole `LocalStorage` contents as a state variable. */
export function useLocalStorageValues(storage: LocalStorage): Partial<LocalStorageData> {
  return useVueValue(
    useCallback(() => storage['values'], [storage]),
    true,
  )
}
