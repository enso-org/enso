/** Unsafe whe the record can have extra keys which are not in `K`. */
export function unsafeEntries<K extends PropertyKey, V>(obj: Record<K, V>): [K, V][] {
  return Object.entries(obj) as any
}
