/** Returns the set of all nodes transitively reachable from the given set of roots */
export function reachable<T>(roots: Iterable<T>, edges: (node: T) => Iterable<T>): Set<T> {
  const toVisit = [...new Set(roots)]
  const result = new Set<T>()

  let current: T | undefined
  while ((current = toVisit.pop())) {
    for (const nextReachable of edges(current)) {
      if (!result.has(nextReachable)) {
        result.add(nextReachable)
        toVisit.push(nextReachable)
      }
    }
  }

  return result
}
