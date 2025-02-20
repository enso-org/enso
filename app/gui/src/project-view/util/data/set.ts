/** TODO: Add docs */
export function intersectionSize(a: Set<unknown>, b: Set<unknown>): number {
  let size = 0
  for (const item of a) if (b.has(item)) size += 1
  return size
}
