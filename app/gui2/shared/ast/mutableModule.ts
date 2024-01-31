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

export interface ModuleUpdate {
  nodesAdded: AstId[]
  nodesDeleted: AstId[]
  fieldsUpdated: { id: AstId; fields: (readonly [string, unknown])[] }[]
  metadataUpdated: { id: AstId; changes: Map<string, unknown> }[]
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
    const doc = new Y.Doc()
    Y.applyUpdateV2(doc, Y.encodeStateAsUpdateV2(this.ydoc))
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

  syncRoot(root: Owned) {
    this.replaceRoot(root)
    this.gc()
  }

  private gc() {
    const live = new Set<AstId>()
    const active = new Array<Ast>()
    let next: Ast | undefined = this.root()
    while (next) {
      for (const child of next.children()) {
        if (child instanceof Ast) active.push(child)
      }
      live.add(next.id)
      next = active.pop()
    }
    const all = Array.from(this.nodes.keys())
    for (const id of all) {
      if (id === ROOT_ID) continue
      assert(isAstId(id))
      if (!live.has(id)) this.nodes.delete(id)
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
    this.nodes.observeDeep((events) => observer(this.observeEvents(events)))
  }

  getStateAsUpdate(): ModuleUpdate {
    const updateBuilder = new UpdateBuilder(this.nodes)
    for (const id of this.nodes.keys()) updateBuilder.addNode(id as AstId)
    return updateBuilder
  }

  applyUpdate(update: Uint8Array, origin?: string): ModuleUpdate | undefined {
    let summary: ModuleUpdate | undefined
    const observer = (events: Y.YEvent<any>[]) => {
      summary = this.observeEvents(events)
    }
    this.nodes.observeDeep(observer)
    Y.applyUpdate(this.ydoc, update, origin)
    this.nodes.unobserveDeep(observer)
    return summary
  }

  private observeEvents(events: Y.YEvent<any>[]): ModuleUpdate {
    const updateBuilder = new UpdateBuilder(this.nodes)
    for (const event of events) {
      if (event.target === this.nodes) {
        // Updates to the node map.
        for (const [key, change] of event.changes.keys) {
          const id = key as AstId
          switch (change.action) {
            case 'add':
              updateBuilder.addNode(id)
              break
            case 'update':
              updateBuilder.updateAllFields(id)
              break
            case 'delete':
              updateBuilder.deleteNode(id)
              break
          }
        }
      } else if (event.target.parent === this.nodes) {
        // Updates to a node's fields.
        assert(event.target instanceof Y.Map)
        const id = event.target.get('id') as AstId
        const node = this.nodes.get(id)
        if (!node) continue
        const changes: (readonly [string, unknown])[] = Array.from(event.changes.keys, ([key]) => [
          key,
          node.get(key as any),
        ])
        updateBuilder.updateFields(id, changes)
      } else if (event.target.parent.parent === this.nodes) {
        // Updates to fields of a metadata object within a node.
        const id = event.target.parent.get('id') as AstId
        const node = this.nodes.get(id)
        if (!node) continue
        const metadata = node.get('metadata') as unknown as Map<string, unknown>
        const changes: (readonly [string, unknown])[] = Array.from(event.changes.keys, ([key]) => [
          key,
          metadata.get(key as any),
        ])
        updateBuilder.updateMetadata(id, changes)
      }
    }
    return updateBuilder
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
    const metadata = new Y.Map<unknown>() as unknown as FixedMap<{}>
    const metadataFields = setAll(metadata, {
      externalId: externalId ?? newExternalId(),
    })
    const fields = setAll(map_, {
      id,
      type: type,
      parent: undefined,
      metadata: metadataFields,
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
export interface RootPointer extends Invalid {}

function newAstId(type: string): AstId {
  return `ast:${type}#${random.uint53()}` as AstId
}
/** Checks whether the input looks like an AstId. */
export function isAstId(value: string): value is AstId {
  return /ast:[A-Za-z]*#[0-9]*/.test(value)
}
export const ROOT_ID = `Root` as AstId

class UpdateBuilder implements ModuleUpdate {
  readonly nodesAdded: AstId[] = []
  readonly nodesDeleted: AstId[] = []
  readonly fieldsUpdated: { id: AstId; fields: (readonly [string, unknown])[] }[] = []
  readonly metadataUpdated: { id: AstId; changes: Map<string, unknown> }[] = []

  private readonly nodes: YNodes

  constructor(nodes: YNodes) {
    this.nodes = nodes
  }

  addNode(id: AstId) {
    this.nodesAdded.push(id)
    this.updateAllFields(id)
  }

  updateAllFields(id: AstId) {
    this.updateFields(id, this.nodes.get(id)!.entries())
  }

  updateFields(id: AstId, changes: Iterable<readonly [string, unknown]>) {
    const fields = new Array<readonly [string, unknown]>()
    let metadataChanges = undefined
    for (const entry of changes) {
      const [key, value] = entry
      if (key === 'metadata') {
        assert(value instanceof Y.Map)
        metadataChanges = new Map<string, unknown>(value.entries())
      } else {
        assert(!(value instanceof Y.AbstractType))
        fields.push(entry)
      }
    }
    if (fields.length !== 0) this.fieldsUpdated.push({ id, fields })
    if (metadataChanges) this.metadataUpdated.push({ id, changes: metadataChanges })
  }

  updateMetadata(id: AstId, changes: Iterable<readonly [string, unknown]>) {
    const changeMap = new Map<string, unknown>()
    for (const [key, value] of changes) changeMap.set(key, value)
    this.metadataUpdated.push({ id, changes: changeMap })
  }

  deleteNode(id: AstId) {
    this.nodesDeleted.push(id)
  }
}
