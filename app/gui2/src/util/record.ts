/** Unsafe whe the record can have extra keys which are not in `K`. */
export function unsafeEntries<K extends PropertyKey, V>(obj: Record<K, V>): [K, V][] {
  return Object.entries(obj) as any
}

/** Unsafe whe the record can have extra keys which are not in `K`. */
export function unsafeKeys<K extends PropertyKey>(obj: Record<K, unknown>): K[] {
  return Object.keys(obj) as any
}
