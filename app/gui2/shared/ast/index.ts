import * as random from 'lib0/random'
import type { ExternalId } from '../yjsModel'
import type { Module } from './mutableModule'
import type { SyncTokenId } from './token'
import type { AstId } from './tree'
import { Ast, MutableAst } from './tree'

export * from './mutableModule'
export * from './parse'
export * from './token'
export * from './tree'

declare const brandOwned: unique symbol
/** Used to mark references required to be unique.
 *
 *  Note that the typesystem cannot stop you from copying an `Owned`,
 *  but that is an easy mistake to see (because it occurs locally).
 *
 *  We can at least require *obtaining* an `Owned`,
 *  which statically prevents the otherwise most likely usage errors when rearranging ASTs.
 */
export type Owned<T = MutableAst> = T & { [brandOwned]: never }
/** @internal */
export function asOwned<T>(t: T): Owned<T> {
  return t as Owned<T>
}

export type NodeChild<T = AstId | SyncTokenId> = { whitespace?: string | undefined; node: T }

export function newExternalId(): ExternalId {
  return random.uuidv4() as ExternalId
}

/** @internal */
export function parentId(ast: Ast): AstId | undefined {
  return ast.fields.get('parent')
}

/** Returns the given IDs, and the IDs of all their ancestors. */
export function subtrees(module: Module, ids: Iterable<AstId>) {
  const subtrees = new Set<AstId>()
  for (const id of ids) {
    let ast = module.get(id)
    while (ast != null && !subtrees.has(ast.id)) {
      subtrees.add(ast.id)
      ast = ast.parent()
    }
  }
  return subtrees
}

/** Returns the IDs of the ASTs that are not descendants of any others in the given set. */
export function subtreeRoots(module: Module, ids: Set<AstId>) {
  const roots = new Array<AstId>()
  for (const id of ids) {
    const astInModule = module.get(id)
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
    if (!hasParentInSet) roots.push(id)
  }
  return roots
}
