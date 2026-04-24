/** @returns whether the provided sets have any key in common. */
export function setsIntersect(
  a: Pick<Set<unknown>, 'keys'> | undefined,
  b: Pick<Set<unknown>, 'has'> | undefined,
) {
  if (a && b) {
    for (const key of a.keys()) {
      if (b.has(key)) return true
    }
  }
  return false
}
