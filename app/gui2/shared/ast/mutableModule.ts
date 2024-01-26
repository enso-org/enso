import * as random from 'lib0/random'
import * as Y from 'yjs'
import type { AstId, Owned, SyncTokenId } from '.'
import { Token, asOwned, isTokenId, newExternalId } from '.'
import { assert, assertDefined } from '../util/assert'
import type { ExternalId, SourceRange } from '../yjsModel'
import type { AstFields, FixedMap, Mutable } from './tree'
import {
  Ast,
  Invalid,
  MutableAst,
  MutableInvalid,
  Wildcard,
  invalidFields,
  materializeMutable,
  setAll,
} from './tree'

export interface Module {
  edit(): MutableModule
  root(): Ast | undefined
  get(id: AstId): Ast | undefined
  get(id: AstId | undefined): Ast | undefined

  /////////////////////////////////

  checkedGet(id: AstId): Ast
  checkedGet(id: AstId | undefined): Ast | undefined
  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined
  getAny(node: AstId | SyncTokenId): Ast | Token
  has(id: AstId): boolean
  getSpan(id: AstId): SourceRange | undefined
}

interface ModuleUpdate {
  addNodes: AstId[]
  deleteNodes: AstId[]
  updateNodes: { id: AstId; fields: (readonly [string, unknown])[] }[]
}

type YNode = FixedMap<AstFields>
type YNodes = Y.Map<YNode>

export class MutableModule implements Module {
  private readonly nodes: YNodes

  get ydoc() {
    const ydoc = this.nodes.doc
    assert(ydoc != null)
    return ydoc
  }

  /** Return this module's copy of `ast`, if this module was created by cloning `ast`'s module. */
  getVersion<T extends Ast>(ast: T): Mutable<T> {
    const instance = this.checkedGet(ast.id)
    return instance as Mutable<T>
  }

  edit(): MutableModule {
    const state = Y.encodeStateAsUpdateV2(this.ydoc)
    const doc = new Y.Doc()
    Y.applyUpdateV2(doc, state)
    return new MutableModule(doc)
  }

  root(): MutableAst | undefined {
    return this.rootPointer()?.expression
  }

  replaceRoot(newRoot: Owned | undefined): Owned | undefined {
    if (newRoot) {
      const rootPointer = this.rootPointer()
      if (rootPointer) {
        return rootPointer.expression.replace(newRoot)
      } else {
        invalidFields(this, this.baseObject('Invalid', undefined, ROOT_ID), {
          whitespace: '',
          node: newRoot,
        })
        return undefined
      }
    } else {
      const oldRoot = this.root()
      if (!oldRoot) return
      this.nodes.delete(ROOT_ID)
      oldRoot.fields.set('parent', undefined)
      return asOwned(oldRoot)
    }
  }

  /** Copy the given node into the module. */
  copy<T extends Ast>(ast: T): Owned<Mutable<T>> {
    const id = newAstId(ast.typeName())
    const fields = ast.fields.clone()
    this.nodes.set(id, fields as any)
    fields.set('id', id)
    fields.set('parent', undefined)
    const ast_ = materializeMutable(this, fields)
    ast_.importReferences(ast.module)
    return ast_ as Owned<Mutable<typeof ast>>
  }

  static Transient() {
    return new this(new Y.Doc())
  }

  observe(observer: (update: ModuleUpdate) => void) {
    this.nodes.observeDeep((events) => {
      const addNodes = []
      const deleteNodes = []
      const updateNodes = []
      for (const event of events) {
        if (event.target === this.nodes) {
          for (const [key, change] of event.changes.keys) {
            const id = key as AstId
            switch (change.action) {
              case 'add':
                addNodes.push(id)
                updateNodes.push({ id, fields: Array.from(this.nodes.get(id)!.entries()) })
                break
              case 'update':
                updateNodes.push({ id, fields: Array.from(this.nodes.get(id)!.entries()) })
                break
              case 'delete':
                deleteNodes.push(id)
                break
            }
          }
        } else {
          assert(event.target.parent === this.nodes)
          assert(event.target instanceof Y.Map)
          const id = event.target.get('id') as AstId
          const node = this.nodes.get(id)
          assertDefined(node)
          const fields: [string, unknown][] = []
          for (const [key, change] of event.changes.keys) {
            switch (change.action) {
              case 'add':
              case 'update': {
                assert((node as Y.Map<unknown>).has(key as any))
                const value: unknown = node.get(key as any)
                fields.push([key, value])
                break
              }
              case 'delete':
                fields.push([key, undefined])
                break
            }
          }
          updateNodes.push({ id, fields })
        }
      }
      observer({ addNodes, deleteNodes, updateNodes })
    })
  }

  clear() {
    this.nodes.clear()
  }

  checkedGet(id: AstId): Mutable
  checkedGet(id: AstId | undefined): Mutable | undefined
  checkedGet(id: AstId | undefined): Mutable | undefined {
    if (!id) return undefined
    const ast = this.get(id)
    assert(ast !== undefined, 'id in module')
    return ast
  }

  get(id: AstId): Mutable | undefined
  get(id: AstId | undefined): Mutable | undefined
  get(id: AstId | undefined): Mutable | undefined {
    if (!id) return undefined
    const nodeData = this.nodes.get(id)
    if (!nodeData) return undefined
    const fields = nodeData as any
    return materializeMutable(this, fields)
  }

  replace(id: AstId, value: Owned): Owned | undefined {
    return this.get(id)?.replace(value)
  }

  replaceValue(id: AstId, value: Owned): Owned | undefined {
    return this.get(id)?.replaceValue(value)
  }

  take(id: AstId): Owned {
    return this.replace(id, Wildcard.new(this)) || asOwned(this.checkedGet(id))
  }

  updateValue<T extends MutableAst>(id: AstId, f: (x: Owned) => Owned<T>): T | undefined {
    return this.get(id)?.updateValue(f)
  }

  /////////////////////////////////////////////

  getSpan(id: AstId) {
    return undefined
  }

  constructor(doc: Y.Doc) {
    this.nodes = doc.getMap<YNode>('nodes')
  }

  private rootPointer(): MutableRootPointer | undefined {
    const rootPointer = this.get(ROOT_ID)
    if (rootPointer) return rootPointer as MutableRootPointer
  }

  /** @internal */
  baseObject(type: string, externalId?: ExternalId, overrideId?: AstId): FixedMap<AstFields> {
    const map = new Y.Map<unknown>()
    const map_ = map as unknown as FixedMap<{}>
    const id = overrideId ?? newAstId(type)
    const fields = setAll(map_, {
      id,
      externalId: externalId ?? newExternalId(),
      type: type,
      parent: undefined,
    })
    this.nodes.set(id, fields)
    return fields
  }

  /** @internal */
  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return Token.withId(token.code_, token.tokenType_, token.id)
  }

  getAny(node: AstId | SyncTokenId): MutableAst | Token {
    return isTokenId(node) ? this.getToken(node) : this.checkedGet(node)
  }

  /** @internal Copy a node into the module, if it is bound to a different module. */
  copyIfForeign<T extends MutableAst>(ast: Owned<T>): Owned<T>
  copyIfForeign<T extends MutableAst>(ast: Owned<T> | undefined): Owned<T> | undefined {
    if (!ast) return ast
    if (ast.module === this) return ast
    return this.copy(ast) as any
  }

  /** @internal */
  delete(id: AstId) {
    this.nodes.delete(id)
  }

  /** @internal */
  has(id: AstId) {
    return this.nodes.has(id)
  }
}

type MutableRootPointer = MutableInvalid & { get expression(): MutableAst | undefined }
/** @internal */
export type RootPointer = Invalid

function newAstId(type: string): AstId {
  return `ast:${type}#${random.uint53()}` as AstId
}
/** Checks whether the input looks like an AstId. */
export function isAstId(value: string): value is AstId {
  return /ast:[A-Za-z]*#[0-9]*/.test(value)
}
export const ROOT_ID = `Root` as AstId
