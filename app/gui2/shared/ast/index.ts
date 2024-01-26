import * as random from 'lib0/random'
import type { ExternalId } from '../yjsModel'
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
