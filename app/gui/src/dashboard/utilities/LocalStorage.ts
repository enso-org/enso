/** @file A LocalStorage data manager. */
import type * as z from 'zod'

import * as common from 'enso-common'

import * as object from '#/utilities/object'
import { IS_DEV_MODE } from 'enso-common/src/detect'
import invariant from 'tiny-invariant'

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

// ===============================
// === LocalStorageKeyMetadata ===
// ===============================

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

// ========================
// === LocalStorageData ===
// ========================

/**
 * The data that can be stored in a {@link LocalStorage}.
 * Declaration merge into this interface to add a new key.
 */
export interface LocalStorageData {}

// =======================
// === LocalStorageKey ===
// =======================

/** All possible keys of a {@link LocalStorage}. */
export type LocalStorageKey = keyof LocalStorageData

// ====================
// === LocalStorage ===
// ====================

/** A LocalStorage data manager. */
export default class LocalStorage {
  // This is UNSAFE. It is assumed that `LocalStorage.register` is always called
  // when `LocalStorageData` is declaration merged into.
  // eslint-disable-next-line no-restricted-syntax
  private static keyMetadata = {} as Record<
    LocalStorageKey,
    LocalStorageKeyMetadata<LocalStorageKey>
  >
  private static instance: LocalStorage | null = null
  localStorageKey = common.PRODUCT_NAME.toLowerCase()
  protected values: Partial<LocalStorageData>
  private readonly eventTarget = new EventTarget()

  /** Create a {@link LocalStorage}. */
  private constructor() {
    this.values = {}
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

  /**
   * Get all registered keys.
   */
  static getAllKeys() {
    // This is SAFE because `LocalStorage.keyMetadata` is a statically known set of keys.
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
    LocalStorage.keyMetadata[key] = metadata
  }

  /** Register runtime behavior associated with a {@link LocalStorageKey}. */
  static register<K extends LocalStorageKey>(metadata: { [K_ in K]: LocalStorageKeyMetadata<K_> }) {
    for (const key in metadata) {
      LocalStorage.registerKey(key, metadata[key])
    }
  }

  /** Retrieve an entry from the stored data. */
  get<K extends LocalStorageKey>(key: K) {
    this.assertRegisteredKey(key)

    if (!(key in this.values)) {
      const value = this.readValueFromLocalStorage(key)

      if (value != null) {
        this.values[key] = value
      }
    }

    return this.values[key]
  }

  /** Write an entry to the stored data, and save. */
  set<K extends LocalStorageKey>(key: K, value: LocalStorageData[K]) {
    this.assertRegisteredKey(key)

    this.values[key] = value

    this.eventTarget.dispatchEvent(new Event(key))
    this.eventTarget.dispatchEvent(new Event('_change'))

    this.save()
  }

  /** Delete an entry from the stored data, and save. */
  delete<K extends LocalStorageKey>(key: K) {
    this.assertRegisteredKey(key)

    const oldValue = this.values[key]
    // The key being deleted is one of a statically known set of keys.
    // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
    delete this.values[key]
    this.eventTarget.dispatchEvent(new Event(key))
    this.eventTarget.dispatchEvent(new Event('_change'))
    this.save()

    return oldValue
  }

  /** Delete user-specific entries from the stored data, and save. */
  clearUserSpecificEntries() {
    for (const [key, metadata] of object.unsafeEntries(LocalStorage.keyMetadata)) {
      if (metadata.isUserSpecific === true) {
        this.delete(key)
      }
    }
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

  /** Add an event listener to all keys. */
  subscribeAll(callback: (value: Partial<LocalStorageData>) => void) {
    const onChange = () => {
      callback(this.values)
    }
    this.eventTarget.addEventListener('_change', onChange)

    return () => {
      this.eventTarget.removeEventListener('_change', onChange)
    }
  }

  /** Save the current value of the stored data.. */
  protected save() {
    localStorage.setItem(this.localStorageKey, JSON.stringify(this.values))
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
  private readValueFromLocalStorage<
    Key extends LocalStorageKey,
    Value extends LocalStorageData[Key],
  >(key: Key): Value | null {
    this.assertRegisteredKey(key)

    const storedValues = localStorage.getItem(this.localStorageKey)
    const savedValues: unknown = JSON.parse(storedValues ?? '{}')

    if (typeof savedValues === 'object' && savedValues != null && key in savedValues) {
      // @ts-expect-error This is SAFE, as it is guarded by the `key in savedValues` check.
      const savedValue: unknown = savedValues[key]
      const parsedValue = LocalStorage.keyMetadata[key].schema.safeParse(savedValue)

      if (parsedValue.success) {
        // This is safe because the schema is validated before this code is reached.
        // eslint-disable-next-line no-restricted-syntax
        return parsedValue.data as Value
      }

      // eslint-disable-next-line no-restricted-properties
      console.warn('LocalStorage failed to parse value', {
        key,
        savedValue,
        error: parsedValue.error,
      })
    }

    return null
  }
}
