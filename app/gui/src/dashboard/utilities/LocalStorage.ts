/** @file A LocalStorage data manager. */
import invariant from 'tiny-invariant'
import type * as z from 'zod'

import { PRODUCT_NAME } from 'enso-common'
import { IS_DEV_MODE } from 'enso-common/src/detect'

import { createObject, unsafeEntries, unsafeKeys } from 'enso-common/src/utilities/data/object'

const KEY_DEFINITION_STACK_TRACES = new Map<string, string>()

/**
 * Whether the source location for `LocalStorage.defineKey(key, metadata)`
 * is different to the previous known source location.
 */
function isSourceChanged(key: string) {
  const stack = (new Error().stack ?? '').replace(/[?]t=\d+:\d+:\d+/g, '')
  const isChanged = stack !== KEY_DEFINITION_STACK_TRACES.get(key)
  KEY_DEFINITION_STACK_TRACES.set(key, stack)
  return isChanged
}

/** Metadata describing runtime behavior associated with a local storage key. */
export interface LocalStorageKeyMetadata {
  readonly isUserSpecific?: boolean
  readonly schema: z.ZodType
}

/** A LocalStorage data manager. */
export class LocalStorage {
  static keyMetadata: Record<string, LocalStorageKeyMetadata> = {}
  private static instance: LocalStorage | null = null
  localStorageKey = PRODUCT_NAME.toLowerCase()
  protected values: Record<string, unknown>
  protected validatedKeys = new Set<string>()
  private readonly eventTarget = new EventTarget()

  /** Create a {@link LocalStorage}. */
  private constructor() {
    const values: unknown = JSON.parse(localStorage.getItem(this.localStorageKey) ?? '{}')
    // eslint-disable-next-line no-restricted-syntax
    this.values = typeof values === 'object' ? ((values ?? {}) as typeof this.values) : {}
  }

  /** Get the singleton instance of {@link LocalStorage}. */
  static getInstance() {
    if (LocalStorage.instance == null) {
      LocalStorage.instance = new LocalStorage()
    }
    return LocalStorage.instance
  }

  /** Register metadata associated with a key. */
  static defineKey(key: string, metadata: LocalStorageKeyMetadata) {
    if (IS_DEV_MODE ? isSourceChanged(key) : true) {
      invariant(
        !(key in LocalStorage.keyMetadata),
        `Local storage key '${key}' has already been registered.`,
      )
    }
    LocalStorage.keyMetadata[key] = metadata
  }

  /** Get all registered keys. */
  static getAllKeys() {
    return unsafeKeys(LocalStorage.keyMetadata)
  }

  /** Retrieve an entry from the stored data. */
  get(key: string) {
    if (!this.validatedKeys.has(key)) {
      this.validatedKeys.add(key)
      const metadata = LocalStorage.keyMetadata[key]
      const value = this.values[key]
      if (key in this.values && metadata) {
        const parsed = metadata.schema.safeParse(value)
        if (!parsed.success) {
          // eslint-disable-next-line no-restricted-properties
          console.warn(`Value for key '${key}' does not match schema:`, value)
          // eslint-disable-next-line no-restricted-properties
          console.warn('Errors:', parsed.error)
          // The key being deleted is one of a statically known set of keys.
          // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
          delete this.values[key]
        }
      }
    }
    return this.values[key]
  }

  /** Write an entry to the stored data, and save. */
  set(key: string, value: unknown) {
    if (Object.is(this.values[key], value)) {
      return
    }
    this.values[key] = value
    this.eventTarget.dispatchEvent(new Event(key))
    this.eventTarget.dispatchEvent(new Event('_change'))
    this.save()
  }

  /** Delete an entry from the stored data, and save. */
  delete(key: string) {
    // The key being deleted is one of a statically known set of keys.
    // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
    delete this.values[key]
    this.eventTarget.dispatchEvent(new Event(key))
    this.eventTarget.dispatchEvent(new Event('_change'))
    this.save()
  }

  /** Read a value from the stored data, and delete it after reading. */
  consume(key: string) {
    const value = this.get(key)
    this.delete(key)
    return value
  }

  /** Delete user-specific entries from the stored data, and save. */
  clearUserSpecificEntries() {
    for (const [key, metadata] of unsafeEntries(LocalStorage.keyMetadata)) {
      if (metadata.isUserSpecific === true) {
        this.delete(key)
      }
    }
  }

  /** Add an event listener to a specific key. */
  subscribe(key: string, callback: (value: unknown) => void) {
    const onChange = () => {
      callback(this.get(key))
    }
    onChange()

    this.eventTarget.addEventListener(key, onChange)

    return () => {
      this.eventTarget.removeEventListener(key, onChange)
    }
  }

  /** Add an event listener to all keys. */
  subscribeAll(callback: (value: Record<string, unknown>) => void) {
    const onChange = () => {
      callback(createObject(this.values))
    }
    onChange()

    this.eventTarget.addEventListener('_change', onChange)

    return () => {
      this.eventTarget.removeEventListener('_change', onChange)
    }
  }

  /** Save the current value of the stored data.. */
  protected save() {
    localStorage.setItem(this.localStorageKey, JSON.stringify(this.values))
  }
}
