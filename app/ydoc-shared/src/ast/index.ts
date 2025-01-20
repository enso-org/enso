import { reachable } from '../util/data/graph'
import type { Module } from './mutableModule'
import type { AstId } from './tree'

export { spanMapToIdMap } from './idMap'
export * from './mutableModule'
export * from './parse'
export { printWithSpans } from './print'
export { repair } from './repair'
export * from './text'
export * from './token'
export * from './tree'

/** Returns the given IDs, and the IDs of all their ancestors. */
export function subtrees(module: Module, ids: Iterable<AstId>) {
  return reachable(ids, (id) => {
    const parent = module.tryGet(id)?.parent()
    return parent ? [id, parent.id] : [id]
  })
}

/** Returns the IDs of the ASTs that are not descendants of any others in the given set. */
export function subtreeRoots(module: Module, ids: Set<AstId>): Set<AstId> {
  const roots = new Set<AstId>()
  for (const id of ids) {
    const astInModule = module.tryGet(id)
    if (!astInModule) continue
    let ast = astInModule.parent()
    let hasParentInSet
    while (ast != null) {
      if (ids.has(ast.id)) {
        hasParentInSet = true
        break
      }
      ast = ast.parent()
    }
    if (!hasParentInSet) roots.add(id)
  }
  return roots
}
