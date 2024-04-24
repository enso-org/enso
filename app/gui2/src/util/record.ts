/** Unsafe whe the record can have extra keys which are not in `K`. */
export function unsafeEntries<K extends PropertyKey, V>(obj: Record<K, V>): [K, V][] {
  return Object.entries(obj) as any
}

/** Unsafe whe the record can have extra keys which are not in `K`. */
export function unsafeKeys<K extends PropertyKey>(obj: Record<K, unknown>): K[] {
  return Object.keys(obj) as any
}

/** Swap keys and value in a record. */
export function swapKeysAndValues<K extends PropertyKey, V extends PropertyKey>(
  record: Record<K, V>,
): Record<V, K> {
  const swappedRecord: Record<V, K> = {} as Record<V, K>
  for (const key in record) {
    const value = record[key]
    swappedRecord[value] = key as K
  }
  return swappedRecord
}
