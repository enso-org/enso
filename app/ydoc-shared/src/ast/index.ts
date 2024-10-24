import * as random from 'lib0/random'
import { reachable } from '../util/data/graph'
import type { ExternalId } from '../yjsModel'
import type { Module } from './mutableModule'
import type { SyncTokenId } from './token'
import type { AstId } from './tree'
import { App, Ast, Group, MutableAst, OprApp, Wildcard } from './tree'

export * from './mutableModule'
export * from './parse'
export * from './text'
export * from './token'
export * from './tree'

declare const brandOwned: unique symbol
/**
 * Used to mark references required to be unique.
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

export type NodeChild<T> = { whitespace: string | undefined; node: T }
export type RawNodeChild = NodeChild<AstId> | NodeChild<SyncTokenId>

/** Create a new random {@link ExternalId}. */
export function newExternalId(): ExternalId {
  return random.uuidv4() as ExternalId
}

/** @internal */
export function parentId(ast: Ast): AstId | undefined {
  return ast.fields.get('parent')
}

/** Returns the given IDs, and the IDs of all their ancestors. */
export function subtrees(module: Module, ids: Iterable<AstId>) {
  return reachable(ids, id => {
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

function unwrapGroups(ast: Ast) {
  while (ast instanceof Group && ast.expression) ast = ast.expression
  return ast
}

/**
 * Tries to recognize inputs that are semantically-equivalent to a sequence of `App`s, and returns the arguments
 *  identified and LHS of the analyzable chain.
 *
 *  In particular, this function currently recognizes syntax used in visualization-preprocessor expressions.
 */
export function analyzeAppLike(ast: Ast): { func: Ast; args: Ast[] } {
  const deferredOperands = new Array<Ast>()
  while (
    ast instanceof OprApp &&
    ast.operator.ok &&
    ast.operator.value.code() === '<|' &&
    ast.lhs &&
    ast.rhs
  ) {
    deferredOperands.push(unwrapGroups(ast.rhs))
    ast = unwrapGroups(ast.lhs)
  }
  deferredOperands.reverse()
  const args = new Array<Ast>()
  while (ast instanceof App) {
    const deferredOperand = ast.argument instanceof Wildcard ? deferredOperands.pop() : undefined
    args.push(deferredOperand ?? unwrapGroups(ast.argument))
    ast = ast.function
  }
  args.reverse()
  return { func: ast, args }
}
