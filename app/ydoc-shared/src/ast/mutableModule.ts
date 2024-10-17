import * as random from 'lib0/random'
import * as Y from 'yjs'
import type { AstId, NodeChild, Owned, RawNodeChild, SyncTokenId } from '.'
import { Token, asOwned, isTokenId, newExternalId, subtreeRoots } from '.'
import { assert, assertDefined } from '../util/assert'
import type { SourceRangeEdit } from '../util/data/text'
import { defaultLocalOrigin, tryAsOrigin, type ExternalId, type Origin } from '../yjsModel'
import type { AstFields, FixedMap, Mutable } from './tree'
import {
  Ast,
  MutableAst,
  MutableInvalid,
  Wildcard,
  composeFieldData,
  invalidFields,
  materializeMutable,
  setAll,
} from './tree'

export interface Module {
  edit(): MutableModule
  root(): Ast | undefined
  tryGet(id: AstId | undefined): Ast | undefined

  /////////////////////////////////

  /** Return the specified AST. Throws an exception if no AST with the provided ID was found. */
  get(id: AstId): Ast
  get(id: AstId | undefined): Ast | undefined
  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined
  getAny(node: AstId | SyncTokenId): Ast | Token
  getConcrete(child: RawNodeChild): NodeChild<Ast> | NodeChild<Token>
  has(id: AstId): boolean
}

export interface ModuleUpdate {
  nodesAdded: Set<AstId>
  nodesDeleted: Set<AstId>
  nodesUpdated: Set<AstId>
  updateRoots: Set<AstId>
  metadataUpdated: { id: AstId; changes: Map<string, unknown> }[]
  origin: Origin | undefined
}

type YNode = FixedMap<AstFields>
type YNodes = Y.Map<YNode>

type UpdateObserver = (update: ModuleUpdate) => void
type YjsObserver = (events: Y.YEvent<any>[], transaction: Y.Transaction) => void

/** TODO: Add docs */
export class MutableModule implements Module {
  private readonly nodes: YNodes
  private updateObservers: UpdateObserver[] | undefined
  private yjsObserver: YjsObserver | undefined

  private get ydoc() {
    const ydoc = this.nodes.doc
    assert(ydoc != null)
    return ydoc
  }

  /** Return this module's copy of `ast`, if this module was created by cloning `ast`'s module. */
  getVersion<T extends Ast>(ast: T): Mutable<T> {
    const instance = this.get(ast.id)
    return instance as Mutable<T>
  }

  /** TODO: Add docs */
  edit(): MutableModule {
    const doc = new Y.Doc()
    Y.applyUpdateV2(doc, Y.encodeStateAsUpdateV2(this.ydoc))
    return new MutableModule(doc)
  }

  /** TODO: Add docs */
  applyEdit(edit: MutableModule, origin: Origin = defaultLocalOrigin) {
    Y.applyUpdateV2(this.ydoc, Y.encodeStateAsUpdateV2(edit.ydoc), origin)
  }

  /** TODO: Add docs */
  transact<T>(f: () => T, origin: Origin = defaultLocalOrigin): T {
    return this.ydoc.transact(f, origin)
  }

  /** TODO: Add docs */
  root(): MutableAst | undefined {
    return this.rootPointer()?.expression
  }

  /** TODO: Add docs */
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

  /** TODO: Add docs */
  syncRoot(root: Owned) {
    this.replaceRoot(root)
    this.gc()
  }

  /** TODO: Add docs */
  syncToCode(code: string) {
    const root = this.root()
    if (root) {
      root.syncToCode(code)
    } else {
      this.replaceRoot(Ast.parse(code, this))
    }
  }

  /** Update the module according to changes to its corresponding source code. */
  applyTextEdits(textEdits: SourceRangeEdit[], metadataSource?: Module) {
    const root = this.root()
    assertDefined(root)
    root.applyTextEdits(textEdits, metadataSource)
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

  /** @internal */
  importCopy<T extends Ast>(ast: T): Owned<Mutable<T>> {
    assert(ast.module !== this)
    ast.visitRecursiveAst(ast => this.nodes.set(ast.id, ast.fields.clone() as any))
    const fields = this.nodes.get(ast.id)
    assertDefined(fields)
    fields.set('parent', undefined)
    return materializeMutable(this, fields) as Owned<Mutable<typeof ast>>
  }

  /** TODO: Add docs */
  static Transient() {
    return new this(new Y.Doc())
  }

  /** TODO: Add docs */
  observe(observer: (update: ModuleUpdate) => void) {
    this.updateObservers ??= []
    this.updateObservers.push(observer)
    this.attachObserver()
    // Attach the observer first, so that if an update hook causes changes in reaction to the initial state update, we
    // won't miss them.
    observer(this.getStateAsUpdate())
    return observer
  }

  private attachObserver() {
    if (this.yjsObserver) return
    this.yjsObserver = (events: Y.YEvent<any>[], transaction: Y.Transaction) => {
      const update = this.observeEvents(events, tryAsOrigin(transaction.origin))
      for (const observer of this.updateObservers ?? []) observer(update)
    }
    this.nodes.observeDeep(this.yjsObserver)
  }

  /** TODO: Add docs */
  unobserve(handle: UpdateObserver) {
    const i = this.updateObservers?.indexOf(handle)
    if (i == null || i < 0) return
    this.updateObservers!.splice(i, 1)
    if (!this.haveObservers()) {
      this.nodes.unobserveDeep(this.yjsObserver!)
      this.yjsObserver = undefined
    }
  }

  private haveObservers() {
    return !!this.updateObservers?.length
  }

  /** TODO: Add docs */
  getStateAsUpdate(): ModuleUpdate {
    const updateBuilder = new UpdateBuilder(this, this.nodes, undefined)
    for (const id of this.nodes.keys()) {
      DEV: assertAstId(id)
      updateBuilder.addNode(id)
    }
    return updateBuilder.finish()
  }

  /** TODO: Add docs */
  applyUpdate(update: Uint8Array, origin: Origin): ModuleUpdate | undefined {
    let summary: ModuleUpdate | undefined
    const observer = (events: Y.YEvent<any>[]) => {
      summary = this.observeEvents(events, origin)
    }
    this.nodes.observeDeep(observer)
    Y.applyUpdate(this.ydoc, update, origin)
    this.nodes.unobserveDeep(observer)
    return summary
  }

  private observeEvents(events: Y.YEvent<any>[], origin: Origin | undefined): ModuleUpdate {
    const updateBuilder = new UpdateBuilder(this, this.nodes, origin)
    for (const event of events) {
      if (event.target === this.nodes) {
        // Updates to the node map.
        for (const [key, change] of event.changes.keys) {
          if (!isAstId(key)) continue
          switch (change.action) {
            case 'add':
              updateBuilder.addNode(key)
              break
            case 'update':
              updateBuilder.updateAllFields(key)
              break
            case 'delete':
              updateBuilder.deleteNode(key)
              break
          }
        }
      } else if (event.target.parent === this.nodes) {
        // Updates to a node's fields.
        assert(event.target instanceof Y.Map)
        const id = event.target.get('id')
        DEV: assertAstId(id)
        const node = this.nodes.get(id)
        if (!node) continue
        const changes: (readonly [string, unknown])[] = Array.from(event.changes.keys, ([key]) => [
          key,
          node.get(key as any),
        ])
        updateBuilder.updateFields(id, changes)
      } else if (event.target.parent.parent === this.nodes) {
        // Updates to fields of a metadata object within a node.
        const id = event.target.parent.get('id')
        DEV: assertAstId(id)
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
    return updateBuilder.finish()
  }

  /** TODO: Add docs */
  clear() {
    this.nodes.clear()
  }

  /** TODO: Add docs */
  get(id: AstId): Mutable
  /** TODO: Add docs */
  get(id: AstId | undefined): Mutable | undefined
  /** TODO: Add docs */
  get(id: AstId | undefined): Mutable | undefined {
    if (!id) return undefined
    const ast = this.tryGet(id)
    assert(ast !== undefined, 'id in module')
    return ast
  }

  /** TODO: Add docs */
  tryGet(id: AstId | undefined): Mutable | undefined {
    if (!id) return undefined
    const nodeData = this.nodes.get(id)
    if (!nodeData) return undefined
    const fields = nodeData as any
    return materializeMutable(this, fields)
  }

  /** TODO: Add docs */
  replace(id: AstId, value: Owned): Owned | undefined {
    return this.tryGet(id)?.replace(value)
  }

  /** TODO: Add docs */
  replaceValue(id: AstId, value: Owned): Owned | undefined {
    return this.tryGet(id)?.replaceValue(value)
  }

  /** TODO: Add docs */
  take(id: AstId): Owned {
    return this.replace(id, Wildcard.new(this)) || asOwned(this.get(id))
  }

  /** TODO: Add docs */
  updateValue<T extends MutableAst>(id: AstId, f: (x: Owned) => Owned<T>): T | undefined {
    return this.tryGet(id)?.updateValue(f)
  }

  /////////////////////////////////////////////

  /** TODO: Add docs */
  constructor(doc: Y.Doc) {
    this.nodes = doc.getMap<YNode>('nodes')
  }

  private rootPointer(): MutableRootPointer | undefined {
    const rootPointer = this.tryGet(ROOT_ID)
    if (rootPointer) return rootPointer as MutableRootPointer
  }

  /** @internal */
  baseObject(type: string, externalId?: ExternalId, overrideId?: AstId): FixedMap<AstFields> {
    const map = new Y.Map()
    const map_ = map as unknown as FixedMap<object>
    const id = overrideId ?? newAstId(type)
    const metadata = new Y.Map() as unknown as FixedMap<object>
    const metadataFields = setAll(metadata, {
      externalId: externalId ?? newExternalId(),
    })
    const fields = setAll(map_, {
      id,
      type: type,
      parent: undefined,
      metadata: metadataFields,
    })
    const fieldObject = composeFieldData(fields, {})
    this.nodes.set(id, fieldObject)
    return fieldObject
  }

  /** @internal */
  getToken(token: SyncTokenId): Token
  /** TODO: Add docs */
  getToken(token: SyncTokenId | undefined): Token | undefined
  /** TODO: Add docs */
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return Token.withId(token.code_, token.tokenType_, token.id)
  }

  /** TODO: Add docs */
  getAny(node: AstId | SyncTokenId): MutableAst | Token {
    return isTokenId(node) ? this.getToken(node) : this.get(node)
  }

  /** TODO: Add docs */
  getConcrete(child: RawNodeChild): NodeChild<Ast> | NodeChild<Token> {
    if (isTokenId(child.node))
      return { whitespace: child.whitespace, node: this.getToken(child.node) }
    else return { whitespace: child.whitespace, node: this.get(child.node) }
  }

  /**
   * Copy a node into the module, if it is bound to a different module.
   * @internal
   */
  copyIfForeign<T extends MutableAst>(ast: Owned<T>): Owned<T>
  /** TODO: Add docs */
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

function newAstId(type: string, sequenceNum = random.uint53()): AstId {
  const id = `ast:${type}#${sequenceNum}`
  DEV: assertAstId(id)
  return id
}

export const __TEST = { newAstId }

/** Checks whether the input looks like an AstId. */
const astIdRegex = /^ast:[A-Za-z]+#[0-9]+$/
/** TODO: Add docs */
export function isAstId(value: string): value is AstId {
  return astIdRegex.test(value)
}

/** TODO: Add docs */
export function assertAstId(value: string): asserts value is AstId {
  assert(isAstId(value), `Incorrect AST ID: ${value}`)
}

export const ROOT_ID = newAstId('Root', 0)

class UpdateBuilder {
  readonly nodesAdded = new Set<AstId>()
  readonly nodesDeleted = new Set<AstId>()
  readonly nodesUpdated = new Set<AstId>()
  readonly metadataUpdated: { id: AstId; changes: Map<string, unknown> }[] = []
  readonly origin: Origin | undefined

  private readonly module: Module
  private readonly nodes: YNodes

  constructor(module: Module, nodes: YNodes, origin: Origin | undefined) {
    this.module = module
    this.nodes = nodes
    this.origin = origin
  }

  addNode(id: AstId) {
    this.nodesAdded.add(id)
  }

  updateAllFields(id: AstId) {
    this.updateFields(id, this.nodes.get(id)!.entries())
  }

  updateFields(id: AstId, changes: Iterable<readonly [string, unknown]>) {
    let fieldsChanged = false
    let metadataChanges = undefined
    for (const entry of changes) {
      const [key, value] = entry
      if (key === 'metadata') {
        assert(value instanceof Y.Map)
        metadataChanges = new Map<string, unknown>(value.entries())
      } else {
        assert(!(value instanceof Y.AbstractType))
        fieldsChanged = true
      }
    }
    if (fieldsChanged) this.nodesUpdated.add(id)
    if (metadataChanges) this.metadataUpdated.push({ id, changes: metadataChanges })
  }

  updateMetadata(id: AstId, changes: Iterable<readonly [string, unknown]>) {
    const changeMap = new Map<string, unknown>()
    for (const [key, value] of changes) changeMap.set(key, value)
    this.metadataUpdated.push({ id, changes: changeMap })
  }

  deleteNode(id: AstId) {
    this.nodesDeleted.add(id)
  }

  finish(): ModuleUpdate {
    const dirtyNodes = new Set(this.nodesUpdated)
    this.nodesAdded.forEach(node => dirtyNodes.add(node))
    const updateRoots = subtreeRoots(this.module, dirtyNodes)
    return { ...this, updateRoots }
  }
}
