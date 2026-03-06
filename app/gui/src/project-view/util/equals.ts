/** Same as `===` operator - used as function parameter. */
export function defaultEquality(a: unknown, b: unknown): boolean {
  return a === b
}

/**
 * Element-wise equality check of arrays.
 * @param a left array
 * @param b right array
 * @param eq equality function for elements. When not specified, `===` operator is used.
 * @returns true if arrays are equal.
 */
export function arrayEquals<T>(
  a: readonly T[],
  b: readonly T[],
  eq: (a: T, b: T) => boolean = defaultEquality,
) {
  if (a === b) return true
  if (a.length !== b.length) return false
  for (let i = 0; i < a.length; ++i) {
    const aVal = a[i]
    const bVal = b[i]
    if ((aVal == undefined) != (bVal == undefined)) return false
    if (aVal != undefined && bVal != undefined && !eq(aVal, bVal)) return false
  }
  return true
}

/**
 * Equal function accepting only primitive values.
 *
 * Used in places where we want to raise typecheck error after changing some field's type
 * to one with nontrivial definition of equality.
 */
export function primitiveEquals(
  a: string | number | boolean | bigint | symbol | undefined | null,
  b: string | number | boolean | bigint | symbol | undefined | null,
): boolean {
  return defaultEquality(a, b)
}
