/** Returns the subset of fields from the provided value that are set to `true` in the mask. */
export function selectFields<T extends object>(
  mask: Readonly<{ [P in keyof T]?: boolean }>,
  value: T,
): Partial<T> {
  return Object.fromEntries(
    Object.entries(mask)
      .filter(([_key, value]) => value)
      .map(([key]) => [key, value[key as keyof T]]),
  ) as Partial<T>
}

/** Returns whether two records have the same keys with the same associated values. */
export function recordEqual<
  T extends Readonly<Record<string, unknown>>,
  U extends Readonly<Record<string, unknown>>,
>(a: T, b: U): boolean {
  const keysA = new Set(Object.keys(a))
  const keysB = new Set(Object.keys(b))
  return keysA.size === keysB.size && Object.entries(a).every(([k, v]) => b[k] === v)
}
