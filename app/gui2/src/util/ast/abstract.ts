import * as RawAst from '@/generated/ast'
import { assert, assertDefined, assertEqual, bail } from '@/util/assert'
import { parseEnso } from '@/util/ast'
import { Err, Ok, type Result } from '@/util/data/result'
import { is_ident_or_operator } from '@/util/ffi'
import type { LazyObject } from '@/util/parserSupport'
import { unsafeEntries } from '@/util/record'
import * as map from 'lib0/map'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import * as Y from 'yjs'
import {
  IdMap,
  isUuid,
  sourceRangeFromKey,
  sourceRangeKey,
  type ExternalId,
  type SourceRange,
  type SourceRangeKey,
} from '../../../shared/yjsModel'

const DEBUG = false

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
function asOwned<T>(t: T): Owned<T> {
  return t as Owned<T>
}

export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn)
  const idMap = spanMapToIdMap(printed.info)
  const module = MutableModule.Transient()
  const tree = parseEnso(printed.code)
  const { root: parsed, spans } = abstract(module, tree, printed.code)
  module.replaceRoot(parsed)
  setExternalIds(module, spans, idMap)
  return parsed
}

export type NodeChild<T = AstId | SyncTokenId> = { whitespace?: string | undefined; node: T }

declare const brandTokenId: unique symbol
declare const brandAstId: unique symbol
export type TokenId = ExternalId & { [brandTokenId]: never }
export type AstId = string & { [brandAstId]: never }

function newExternalId(): ExternalId {
  return random.uuidv4() as ExternalId
}
function newTokenId(): TokenId {
  return newExternalId() as TokenId
}
function newAstId(type: string): AstId {
  return `ast:${type}#${random.uint53()}` as AstId
}
/** Checks whether the input looks like an AstId. */
export function isAstId(value: string): value is AstId {
  return /ast:[A-Za-z]*#[0-9]*/.test(value)
}
export const ROOT_ID = `Root` as AstId

/** @internal */
export interface SyncTokenId {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined
}
export class Token implements SyncTokenId {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined

  private constructor(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    this.id = id
    this.code_ = code
    this.tokenType_ = type
  }

  get externalId(): TokenId {
    return this.id
  }

  static new(code: string, type?: RawAst.Token.Type) {
    return new this(code, type, newTokenId())
  }

  static withId(code: string, type: RawAst.Token.Type | undefined, id: TokenId) {
    assert(isUuid(id))
    return new this(code, type, id)
  }

  code(): string {
    return this.code_
  }

  typeName(): string {
    if (this.tokenType_) return RawAst.Token.typeNames[this.tokenType_]!
    else return 'Raw'
  }
}
// We haven't had much need to distinguish token types, but it's useful to know that an identifier token's code is a
// valid string for an identifier.
export interface IdentifierOrOperatorIdentifierToken extends Token {
  code(): IdentifierOrOperatorIdentifier
}
export interface IdentifierToken extends Token {
  code(): Identifier
}

declare const qualifiedNameBrand: unique symbol
declare const identifierBrand: unique symbol
declare const operatorBrand: unique symbol

/** A string representing a valid qualified name of our language.
 *
 * In our language, the segments are separated by `.`. All the segments except the last must be lexical identifiers. The
 * last may be an identifier or a lexical operator. A single identifier is also a valid qualified name.
 */
export type QualifiedName = string & { [qualifiedNameBrand]: never }

/** A string representing a lexical identifier. */
export type Identifier = string & { [identifierBrand]: never; [qualifiedNameBrand]: never }

/** A string representing a lexical operator. */
export type Operator = string & { [operatorBrand]: never; [qualifiedNameBrand]: never }

/** A string that can be parsed as an identifier in some contexts.
 *
 *  If it is lexically an identifier (see `StrictIdentifier`), it can be used as identifier anywhere.
 *
 *  If it is lexically an operator (see `Operator`), it takes the syntactic role of an identifier if it is the RHS of
 *  a `PropertyAccess`, or it is the name of a `Function` being defined within a type. In all other cases, it is not
 *  valid to use a lexical operator as an identifier (rather, it will usually parse as an `OprApp` or `UnaryOprApp`).
 */
export type IdentifierOrOperatorIdentifier = Identifier | Operator

/** Returns true if `code` can be used as an identifier in some contexts.
 *
 *  If it is lexically an identifier (see `isStrictIdentifier`), it can be used as identifier anywhere.
 *
 *  If it is lexically an operator (see `isOperator`), it takes the syntactic role of an identifier if it is the RHS of
 *  a `PropertyAccess`, or it is the name of a `Function` being defined within a type. In all other cases, it is not
 *  valid to use a lexical operator as an identifier (rather, it will usually parse as an `OprApp` or `UnaryOprApp`).
 */
export function isIdentifierOrOperatorIdentifier(
  code: string,
): code is IdentifierOrOperatorIdentifier {
  return is_ident_or_operator(code) !== 0
}

/** Returns true if `code` is lexically an identifier. */
export function isIdentifier(code: string): code is Identifier {
  return is_ident_or_operator(code) === 1
}

/** Returns true if `code` is lexically an operator. */
export function isOperator(code: string): code is Operator {
  return is_ident_or_operator(code) === 2
}

function clone<T>(value: T): T {
  if (value instanceof Array) {
    return Array.from(value, clone) as T
  }
  if (value && typeof value === 'object') {
    return cloneObject(value)
  }
  return value
}
function cloneObject<T extends Object>(object: T): T {
  const mapEntry = ([name, value]: [string, unknown]) => [name, clone(value)]
  const properties = Object.fromEntries(Object.entries(object).map(mapEntry))
  return Object.assign(Object.create(Object.getPrototypeOf(object)), properties)
}

function spaced<T extends object | string>(node: T): NodeChild<T>
function spaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function spaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { whitespace: ' ', node }
}

function unspaced<T extends object | string>(node: T): NodeChild<T>
function unspaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function unspaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { whitespace: '', node }
}

function autospaced<T extends object | string>(node: T): NodeChild<T>
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined
function autospaced<T extends object | string>(node: T | undefined): NodeChild<T> | undefined {
  if (node === undefined) return node
  return { node }
}

function spacedIf<T>(node: T, isSpaced: boolean): NodeChild<T> {
  return { whitespace: isSpaced ? ' ' : '', node }
}

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
        invalidFields(this, this.baseObject('Invalid', undefined, ROOT_ID), unspaced(newRoot))
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

export type Mutable<T extends Ast = Ast> = T extends App
  ? MutableApp
  : T extends Assignment
  ? MutableAssignment
  : T extends BodyBlock
  ? MutableBodyBlock
  : T extends Documented
  ? MutableDocumented
  : T extends Function
  ? MutableFunction
  : T extends Generic
  ? MutableGeneric
  : T extends Group
  ? MutableGroup
  : T extends Ident
  ? MutableIdent
  : T extends Import
  ? MutableImport
  : T extends Invalid
  ? MutableInvalid
  : T extends NegationApp
  ? MutableNegationApp
  : T extends NumericLiteral
  ? MutableNumericLiteral
  : T extends OprApp
  ? MutableOprApp
  : T extends PropertyAccess
  ? MutablePropertyAccess
  : T extends TextLiteral
  ? MutableTextLiteral
  : T extends UnaryOprApp
  ? MutableUnaryOprApp
  : T extends Wildcard
  ? MutableWildcard
  : MutableAst

function materializeMutable(module: MutableModule, fields: FixedMap<AstFields>): MutableAst {
  const type = fields.get('type')
  switch (type) {
    case 'App':
      return new MutableApp(module, fields)
    case 'UnaryOprApp':
      return new MutableUnaryOprApp(module, fields)
    case 'NegationApp':
      return new MutableNegationApp(module, fields)
    case 'OprApp':
      return new MutableOprApp(module, fields)
    case 'PropertyAccess':
      return new MutablePropertyAccess(module, fields)
    case 'Generic':
      return new MutableGeneric(module, fields)
    case 'Import':
      return new MutableImport(module, fields)
    case 'TextLiteral':
      return new MutableTextLiteral(module, fields)
    case 'Documented':
      return new MutableDocumented(module, fields)
    case 'Invalid':
      return new MutableInvalid(module, fields)
    case 'Group':
      return new MutableGroup(module, fields)
    case 'NumericLiteral':
      return new MutableNumericLiteral(module, fields)
    case 'Function':
      return new MutableFunction(module, fields)
    case 'Assignment':
      return new MutableAssignment(module, fields)
    case 'BodyBlock':
      return new MutableBodyBlock(module, fields)
    case 'Ident':
      return new MutableIdent(module, fields)
    case 'Wildcard':
      return new MutableWildcard(module, fields)
  }
  bail(`Invalid type: ${type}`)
}

/** @internal */
export type FixedMapView<Fields> = {
  get<Key extends string & keyof Fields>(key: Key): Fields[Key]
  entries(): IterableIterator<readonly [string, unknown]>
  clone(): FixedMap<Fields>
}

type FixedMap<Fields> = FixedMapView<Fields> & {
  set<Key extends string & keyof Fields>(key: Key, value: Fields[Key]): void
}

function getAll<Fields extends object>(map: FixedMapView<Fields>): Fields {
  return Object.fromEntries(map.entries()) as Fields
}

/** Modifies the input `map`. Returns the same object with an extended type. */
function setAll<Fields1, Fields2 extends object>(
  map: FixedMap<Fields1>,
  fields: Fields2,
): FixedMap<Fields1 & Fields2> {
  const map_ = map as FixedMap<Fields1 & Fields2>
  for (const [k, v] of Object.entries(fields)) {
    const k_ = k as string & (keyof Fields1 | keyof Fields2)
    map_.set(k_, v)
  }
  return map_
}

type Removed<T extends MutableAst> = { node: Owned<T>; placeholder: MutableWildcard | undefined }

/** @internal */
export type AstFields = {
  id: AstId
  externalId: ExternalId
  type: string
  parent: AstId | undefined
}
function parentId(ast: Ast): AstId | undefined {
  return ast.fields.get('parent')
}

/** @internal */
export function isTokenId(t: SyncTokenId | AstId | Ast | Owned<Ast> | Owned): t is SyncTokenId {
  return typeof t === 'object' && !(t instanceof Ast)
}

function isToken(t: unknown): t is Token {
  return t instanceof Token
}

export abstract class Ast {
  readonly module: Module
  /** @internal */
  readonly fields: FixedMapView<AstFields>

  get id(): AstId {
    return this.fields.get('id')
  }

  get externalId(): ExternalId {
    const id = this.fields.get('externalId')
    assert(id != null)
    return id
  }

  typeName(): string {
    return this.fields.get('type')
  }

  /**
   * Return whether `this` and `other` are the same object, possibly in different modules.
   */
  is<T extends Ast>(other: T): boolean {
    return this.id === other.id
  }

  /** Return this node's span, if it belongs to a module with an associated span map. */
  get span(): SourceRange | undefined {
    return this.module.getSpan(this.id)
  }

  innerExpression(): Ast {
    // TODO: Override this in `Documented`, `Annotated`, `AnnotatedBuiltin`
    return this
  }

  code(): string {
    return print(this).code
  }

  visitRecursive(visit: (node: Ast | Token) => void): void {
    visit(this)
    for (const child of this.children()) {
      if (isToken(child)) {
        visit(child)
      } else {
        child.visitRecursive(visit)
      }
    }
  }

  visitRecursiveAst(visit: (ast: Ast) => void): void {
    visit(this)
    for (const child of this.children()) {
      if (!isToken(child)) child.visitRecursiveAst(visit)
    }
  }

  printSubtree(info: SpanMap, offset: number, parentIndent: string | undefined): string {
    let code = ''
    for (const child of this.concreteChildren()) {
      if (!isTokenId(child.node) && this.module.checkedGet(child.node) === undefined) continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        code += ' '
      }
      if (isTokenId(child.node)) {
        const tokenStart = offset + code.length
        const token = this.module.getToken(child.node)
        const span = tokenKey(tokenStart, token.code().length)
        info.tokens.set(span, token)
        code += token.code()
      } else {
        const childNode = this.module.checkedGet(child.node)
        assert(childNode != null)
        code += childNode.printSubtree(info, offset + code.length, parentIndent)
        // Extra structural validation.
        assertEqual(childNode.id, child.node)
        if (parentId(childNode) !== this.id) {
          console.error(`Inconsistent parent pointer (expected ${this.id})`, childNode)
        }
        assertEqual(parentId(childNode), this.id)
      }
    }
    const span = nodeKey(offset, code.length)
    const infos = map.setIfUndefined(info.nodes, span, (): Ast[] => [])
    infos.push(this)
    return code
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Token> {
    for (const child of this.concreteChildren()) {
      if (isTokenId(child.node)) {
        yield this.module.getToken(child.node)
      } else {
        const node = this.module.checkedGet(child.node)
        if (node) yield node
      }
    }
  }

  parent(): Ast | undefined {
    const parentId = this.fields.get('parent')
    if (parentId === 'ROOT_ID') return
    return this.module.checkedGet(parentId)
  }

  static parseBlock(source: string, inModule?: MutableModule) {
    return parseBlock(source, inModule)
  }

  static parse(source: string, module?: MutableModule) {
    return parse(source, module)
  }

  ////////////////////

  protected constructor(module: Module, fields: FixedMapView<AstFields>) {
    this.module = module
    this.fields = fields
  }

  /** @internal
   *  Returns child subtrees, including information about the whitespace between them.
   */
  abstract concreteChildren(): IterableIterator<NodeChild>
}

export interface MutableAst {}
export abstract class MutableAst extends Ast {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields>

  setExternalId(id: ExternalId) {
    this.fields.set('externalId', id)
  }

  /** Modify the parent of this node to refer to a new object instead. Return the object, which now has no parent. */
  replace<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const parentId = this.fields.get('parent')
    if (parentId) {
      const parent = this.module.checkedGet(parentId)
      parent.replaceChild(this.id, replacement)
      this.fields.set('parent', undefined)
    }
    return asOwned(this)
  }

  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const replacement_ = this.module.copyIfForeign(replacement)
    const old = this.replace(replacement_)
    replacement_.setExternalId(old.externalId)
    old.setExternalId(newExternalId())
    return old
  }

  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless, and the placeholder. */
  takeToReplace(): Removed<this> {
    if (parentId(this)) {
      const placeholder = Wildcard.new(this.module)
      const node = this.replace(placeholder)
      return { node, placeholder }
    } else {
      return { node: asOwned(this), placeholder: undefined }
    }
  }

  /** Replace the parent of this object with a reference to a new placeholder object.
   *  Returns the object, now parentless. */
  take(): Owned<this> {
    return this.replace(Wildcard.new(this.module))
  }

  takeIfParented<T extends MutableAst>(): Owned<typeof this> {
    const parent = parentId(this)
    if (parent) {
      const parentAst = this.module.checkedGet(parent)
      const placeholder = Wildcard.new(this.module)
      parentAst.replaceChild(this.id, placeholder)
      this.fields.set('parent', undefined)
    }
    return asOwned(this)
  }

  /** Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   **/
  takeValue(): Removed<typeof this> {
    const placeholder = Wildcard.new(this.module)
    const node = this.replaceValue(placeholder)
    return { node, placeholder }
  }

  /** Take this node from the tree, and replace it with the result of applying the given function to it.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  update<T extends MutableAst>(f: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.takeToReplace()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = f(taken.node)
    taken.placeholder.replace(replacement)
    return replacement
  }

  /** Take this node from the tree, and replace it with the result of applying the given function to it; transfer the
   *  metadata from this node to the replacement.
   *
   *  Note that this is a modification of the *parent* node. Any `Ast` objects or `AstId`s that pointed to the old value
   *  will still point to the old value.
   */
  updateValue<T extends MutableAst>(f: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.takeValue()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = f(taken.node)
    taken.placeholder.replaceValue(replacement)
    return replacement
  }

  mutableParent(): MutableAst | undefined {
    const parentId = this.fields.get('parent')
    if (parentId === 'ROOT_ID') return
    return this.module.checkedGet(parentId)
  }

  ///////////////////

  /** @internal */
  importReferences(module: Module) {
    if (module === this.module) return
    for (const child of this.concreteChildren()) {
      if (!isTokenId(child.node)) {
        const childInForeignModule = module.checkedGet(child.node)
        assert(childInForeignModule !== undefined)
        const importedChild = this.module.copy(childInForeignModule)
        importedChild.fields.set('parent', undefined)
        this.replaceChild(child.node, asOwned(importedChild))
      }
    }
  }

  /** @internal */
  abstract replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>): void

  protected claimChild<T extends MutableAst>(child: Owned<T>): AstId
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): AstId | undefined {
    return child ? claimChild(this.module, child, this.id) : undefined
  }
}

function applyMixins(derivedCtor: any, constructors: any[]) {
  constructors.forEach((baseCtor) => {
    Object.getOwnPropertyNames(baseCtor.prototype).forEach((name) => {
      Object.defineProperty(
        derivedCtor.prototype,
        name,
        Object.getOwnPropertyDescriptor(baseCtor.prototype, name) || Object.create(null),
      )
    })
  })
}

function claimChild<T extends MutableAst>(
  module: MutableModule,
  child: Owned<T>,
  parent: AstId,
): AstId {
  if (child.module === module) assertEqual(child.fields.get('parent'), undefined)
  const child_ = module.copyIfForeign(child)
  child_.fields.set('parent', parent)
  return child_.id
}

function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned>,
  parent: AstId,
): NodeChild<AstId>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned> | undefined,
  parent: AstId,
): NodeChild<AstId> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token>,
  parent: AstId,
): NodeChild<AstId | Token>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId | Token> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned | Token> | undefined,
  parent: AstId,
): NodeChild<AstId | Token> | undefined {
  if (!child) return undefined
  if (isTokenId(child.node)) return child as NodeChild<Token>
  return { ...child, node: claimChild(module, child.node, parent) }
}

type StrictIdentLike = Identifier | IdentifierToken
function toIdentStrict(ident: StrictIdentLike): IdentifierToken
function toIdentStrict(ident: StrictIdentLike | undefined): IdentifierToken | undefined
function toIdentStrict(ident: StrictIdentLike | undefined): IdentifierToken | undefined {
  return ident
    ? isToken(ident)
      ? ident
      : (Token.new(ident, RawAst.Token.Type.Ident) as IdentifierToken)
    : undefined
}

type IdentLike = IdentifierOrOperatorIdentifier | IdentifierOrOperatorIdentifierToken
function toIdent(ident: IdentLike): IdentifierOrOperatorIdentifierToken
function toIdent(ident: IdentLike | undefined): IdentifierOrOperatorIdentifierToken | undefined
function toIdent(ident: IdentLike | undefined): IdentifierOrOperatorIdentifierToken | undefined {
  return ident
    ? isToken(ident)
      ? ident
      : (Token.new(ident, RawAst.Token.Type.Ident) as IdentifierOrOperatorIdentifierToken)
    : undefined
}

function makeEquals(): Token {
  return Token.new('=', RawAst.Token.Type.Operator)
}

function nameSpecification(
  name: StrictIdentLike | undefined,
): { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined {
  return name && { name: autospaced(toIdentStrict(name)), equals: unspaced(makeEquals()) }
}

type AppFields = {
  function: NodeChild<AstId>
  parens: { open: NodeChild<SyncTokenId>; close: NodeChild<SyncTokenId> } | undefined
  nameSpecification: { name: NodeChild<SyncTokenId>; equals: NodeChild<SyncTokenId> } | undefined
  argument: NodeChild<AstId>
}

export class App extends Ast {
  declare fields: FixedMap<AstFields & AppFields>

  constructor(module: Module, fields: FixedMapView<AstFields & AppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    func: NodeChild<Owned>,
    parens: { open: NodeChild<Token>; close: NodeChild<Token> } | undefined,
    nameSpecification: { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined,
    argument: NodeChild<Owned>,
  ) {
    const base = module.baseObject('App')
    const id_ = base.get('id')
    const fields = setAll(base, {
      function: concreteChild(module, func, id_),
      parens,
      nameSpecification,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableApp(module, fields))
  }

  static new(
    module: MutableModule,
    func: Owned,
    argumentName: StrictIdentLike | undefined,
    argument: Owned,
  ) {
    const name = nameSpecification(argumentName)
    return App.concrete(module, unspaced(func), undefined, name, {
      node: argument,
      whitespace: name ? '' : ' ',
    })
  }

  get function(): Ast {
    return this.module.checkedGet(this.fields.get('function').node)
  }
  get argumentName(): Token | undefined {
    return this.module.getToken(this.fields.get('nameSpecification')?.name.node)
  }
  get argument(): Ast {
    return this.module.checkedGet(this.fields.get('argument').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { function: function_, parens, nameSpecification, argument } = getAll(this.fields)
    yield function_
    if (parens) yield parens.open
    if (nameSpecification) {
      yield nameSpecification.name
      yield nameSpecification.equals
    }
    yield argument
    if (parens) yield parens.close
  }
}

type KeysOfFieldType<Fields, T> = {
  [K in keyof Fields]: Fields[K] extends T ? K : never
}[keyof Fields]
function setNode<Fields, Key extends string & KeysOfFieldType<Fields, NodeChild<AstId>>>(
  map: FixedMap<Fields>,
  key: Key,
  node: AstId,
): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<AstId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: AstId | undefined): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<AstId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: AstId | undefined): void {
  // The signature correctly only allows this function to be called if `Fields[Key] instanceof NodeChild<SyncId>`,
  // but it doesn't prove that property to TSC, so we have to cast here.
  const old = map.get(key as string & keyof Fields)
  const updated = old ? { ...old, node } : autospaced(node)
  map.set(key, updated as Fields[Key])
}

export class MutableApp extends App implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AppFields>

  setFunction<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'function', this.claimChild(value))
  }
  setArgumentName(name: StrictIdentLike | undefined) {
    this.fields.set('nameSpecification', nameSpecification(name))
  }
  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('function').node === target) {
      this.setFunction(replacement)
    } else if (this.fields.get('argument').node === target) {
      this.setArgument(replacement)
    }
  }
}
export interface MutableApp extends App, MutableAst {
  get function(): MutableAst
  get argument(): MutableAst
}
applyMixins(MutableApp, [MutableAst])

type UnaryOprAppFields = {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId> | undefined
}
export class UnaryOprApp extends Ast {
  declare fields: FixedMapView<AstFields & UnaryOprAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & UnaryOprAppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('UnaryOprApp')
    const id_ = base.get('id')
    const fields = setAll(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableUnaryOprApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned | undefined) {
    return this.concrete(module, unspaced(operator), argument ? autospaced(argument) : undefined)
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('argument')?.node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }
}

export class MutableUnaryOprApp extends UnaryOprApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & UnaryOprAppFields>

  setOperator(value: Token) {
    this.fields.set('operator', unspaced(value))
  }
  setArgument<T extends MutableAst>(argument: Owned<T> | undefined) {
    setNode(this.fields, 'argument', this.claimChild(argument))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('argument')?.node === target) {
      this.setArgument(replacement)
    }
  }
}
export interface MutableUnaryOprApp extends UnaryOprApp, MutableAst {
  get argument(): MutableAst | undefined
}
applyMixins(MutableUnaryOprApp, [MutableAst])

type NegationAppFields = {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<AstId>
}
export class NegationApp extends Ast {
  declare fields: FixedMapView<AstFields & NegationAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & NegationAppFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, operator: NodeChild<Token>, argument: NodeChild<Owned>) {
    const base = module.baseObject('NegationApp')
    const id_ = base.get('id')
    const fields = setAll(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableNegationApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned) {
    return this.concrete(module, unspaced(operator), autospaced(argument))
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast {
    return this.module.checkedGet(this.fields.get('argument').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }
}

export class MutableNegationApp extends NegationApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NegationAppFields>

  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('argument')?.node === target) {
      this.setArgument(replacement)
    }
  }
}
export interface MutableNegationApp extends NegationApp, MutableAst {
  get argument(): MutableAst
}
applyMixins(MutableNegationApp, [MutableAst])

type OprAppFields = {
  lhs: NodeChild<AstId> | undefined
  operators: NodeChild<SyncTokenId>[]
  rhs: NodeChild<AstId> | undefined
}
export class OprApp extends Ast {
  declare fields: FixedMapView<AstFields & OprAppFields>
  constructor(module: Module, fields: FixedMapView<AstFields & OprAppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned> | undefined,
    operators: NodeChild<Token>[],
    rhs: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('OprApp')
    const id_ = base.get('id')
    const fields = setAll(base, {
      lhs: concreteChild(module, lhs, id_),
      operators,
      rhs: concreteChild(module, rhs, id_),
    })
    return asOwned(new MutableOprApp(module, fields))
  }

  static new(
    module: MutableModule,
    lhs: Owned | undefined,
    operator: Token,
    rhs: Owned | undefined,
  ) {
    return OprApp.concrete(module, unspaced(lhs), [autospaced(operator)], autospaced(rhs))
  }

  get lhs(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('lhs')?.node)
  }
  get operator(): Result<Token, NodeChild<Token>[]> {
    const operators = this.fields.get('operators')
    const operators_ = operators.map((child) => ({
      ...child,
      node: this.module.getToken(child.node),
    }))
    const [opr] = operators_
    return opr ? Ok(opr.node) : Err(operators_)
  }
  get rhs(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('rhs')?.node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { lhs, operators, rhs } = getAll(this.fields)
    if (lhs) yield lhs
    yield* operators
    if (rhs) yield rhs
  }
}

export class MutableOprApp extends OprApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & OprAppFields>

  setLhs<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setOperator(value: Token) {
    this.fields.set('operators', [unspaced(value)])
  }
  setRhs<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'rhs', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('lhs')?.node === target) {
      this.setLhs(replacement)
    } else if (this.fields.get('rhs')?.node === target) {
      this.setRhs(replacement)
    }
  }
}
export interface MutableOprApp extends OprApp, MutableAst {
  get lhs(): MutableAst | undefined
  get rhs(): MutableAst | undefined
}
applyMixins(MutableOprApp, [MutableAst])

type PropertyAccessFields = {
  lhs: NodeChild<AstId> | undefined
  operator: NodeChild<SyncTokenId>
  rhs: NodeChild<AstId>
}
export class PropertyAccess extends Ast {
  declare fields: FixedMapView<AstFields & PropertyAccessFields>
  constructor(module: Module, fields: FixedMapView<AstFields & PropertyAccessFields>) {
    super(module, fields)
  }

  static new(module: MutableModule, lhs: Owned, rhs: IdentLike) {
    const dot = unspaced(Token.new('.', RawAst.Token.Type.Operator))
    return this.concrete(
      module,
      unspaced(lhs),
      dot,
      unspaced(Ident.newAllowingOperators(module, toIdent(rhs))),
    )
  }

  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[]],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  static Sequence(
    segments: [StrictIdentLike, ...StrictIdentLike[], IdentLike],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  static Sequence(
    segments: IdentLike[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
  static Sequence(
    segments: IdentLike[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined {
    let path: Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
    let operatorInNonFinalSegment = false
    segments.forEach((s, i) => {
      const t = toIdent(s)
      if (i !== segments.length - 1 && !isIdentifier(t.code())) operatorInNonFinalSegment = true
      path = path ? this.new(module, path, t) : Ident.newAllowingOperators(module, t)
    })
    if (!operatorInNonFinalSegment) return path
  }

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned> | undefined,
    operator: NodeChild<Token>,
    rhs: NodeChild<Owned<MutableIdent>>,
  ) {
    const base = module.baseObject('PropertyAccess')
    const id_ = base.get('id')
    const fields = setAll(base, {
      lhs: concreteChild(module, lhs, id_),
      operator,
      rhs: concreteChild(module, rhs, id_),
    })
    return asOwned(new MutablePropertyAccess(module, fields))
  }

  get lhs(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('lhs')?.node)
  }
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get rhs(): IdentifierOrOperatorIdentifierToken {
    const ast = this.module.checkedGet(this.fields.get('rhs').node)
    assert(ast instanceof Ident)
    return ast.token as IdentifierOrOperatorIdentifierToken
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { lhs, operator, rhs } = getAll(this.fields)
    if (lhs) yield lhs
    yield operator
    yield rhs
  }
}
export class MutablePropertyAccess extends PropertyAccess implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & PropertyAccessFields>

  setLhs<T extends MutableAst>(value: Owned<T> | undefined) {
    setNode(this.fields, 'lhs', this.claimChild(value))
  }
  setRhs(ident: IdentLike) {
    const node = this.claimChild(Ident.newAllowingOperators(this.module, ident))
    const old = this.fields.get('rhs')
    this.fields.set('rhs', old ? { ...old, node } : unspaced(node))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('lhs')?.node === target) {
      this.setLhs(replacement)
    } else if (this.fields.get('rhs')?.node === target) {
      assert(replacement instanceof MutableIdent)
      this.setRhs(replacement.token)
    }
  }
}
export interface MutablePropertyAccess extends PropertyAccess, MutableAst {
  get lhs(): MutableAst | undefined
}
applyMixins(MutablePropertyAccess, [MutableAst])

type GenericFields = {
  children: NodeChild[]
}
export class Generic extends Ast {
  declare fields: FixedMapView<AstFields & GenericFields>
  constructor(module: Module, fields: FixedMapView<AstFields & GenericFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, children: NodeChild<Owned | Token>[]) {
    const base = module.baseObject('Generic')
    const id_ = base.get('id')
    const fields = setAll(base, {
      children: children.map((child) => concreteChild(module, child, id_)),
    })
    return asOwned(new MutableGeneric(module, fields))
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.fields.get('children')[Symbol.iterator]()
  }
}

export class MutableGeneric extends Generic implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GenericFields>

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const replacement_ = autospaced(this.claimChild(replacement))
    this.fields.set(
      'children',
      this.fields.get('children').map((child) => (child.node === target ? replacement_ : child)),
    )
  }
}
export interface MutableGeneric extends Generic, MutableAst {}
applyMixins(MutableGeneric, [MutableAst])

type RawMultiSegmentAppSegment = {
  header: NodeChild<Token>
  body: NodeChild<AstId> | undefined
}
type OwnedMultiSegmentAppSegment = {
  header: NodeChild<Token>
  body: NodeChild<Owned> | undefined
}
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T>,
): OwnedMultiSegmentAppSegment
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T> | undefined,
): OwnedMultiSegmentAppSegment | undefined
function multiSegmentAppSegment<T extends MutableAst>(
  header: string,
  body: Owned<T> | undefined,
): OwnedMultiSegmentAppSegment | undefined {
  return {
    header: { node: Token.new(header, RawAst.Token.Type.Ident) },
    body: spaced(body ? (body as any) : undefined),
  }
}

function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: AstId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: AstId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: AstId,
): RawMultiSegmentAppSegment | undefined
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: AstId,
): RawMultiSegmentAppSegment | undefined {
  if (!msas) return undefined
  return {
    ...msas,
    body: concreteChild(module, msas.body, parent),
  }
}

type ImportFields = {
  polyglot: RawMultiSegmentAppSegment | undefined
  from: RawMultiSegmentAppSegment | undefined
  import: RawMultiSegmentAppSegment
  all: NodeChild<SyncTokenId> | undefined
  as: RawMultiSegmentAppSegment | undefined
  hiding: RawMultiSegmentAppSegment | undefined
}
export class Import extends Ast {
  declare fields: FixedMapView<AstFields & ImportFields>
  constructor(module: Module, fields: FixedMapView<AstFields & ImportFields>) {
    super(module, fields)
  }

  get polyglot(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('polyglot')?.body?.node)
  }
  get from(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('from')?.body?.node)
  }
  get import_(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('import').body?.node)
  }
  get all(): Token | undefined {
    return this.module.getToken(this.fields.get('all')?.node)
  }
  get as(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('as')?.body?.node)
  }
  get hiding(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('hiding')?.body?.node)
  }

  static concrete(
    module: MutableModule,
    polyglot: OwnedMultiSegmentAppSegment | undefined,
    from: OwnedMultiSegmentAppSegment | undefined,
    import_: OwnedMultiSegmentAppSegment,
    all: NodeChild<Token> | undefined,
    as: OwnedMultiSegmentAppSegment | undefined,
    hiding: OwnedMultiSegmentAppSegment | undefined,
  ) {
    const base = module.baseObject('Import')
    const id_ = base.get('id')
    const fields = setAll(base, {
      polyglot: multiSegmentAppSegmentToRaw(module, polyglot, id_),
      from: multiSegmentAppSegmentToRaw(module, from, id_),
      import: multiSegmentAppSegmentToRaw(module, import_, id_),
      all,
      as: multiSegmentAppSegmentToRaw(module, as, id_),
      hiding: multiSegmentAppSegmentToRaw(module, hiding, id_),
    })
    return asOwned(new MutableImport(module, fields))
  }

  static Qualified(path: IdentLike[], module: MutableModule): Owned<MutableImport> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    return MutableImport.concrete(
      module,
      undefined,
      undefined,
      multiSegmentAppSegment('import', path_),
      undefined,
      undefined,
      undefined,
    )
  }

  static Unqualified(
    path: IdentLike[],
    name: IdentLike,
    module: MutableModule,
  ): Owned<MutableImport> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const name_ = Ident.newAllowingOperators(module, name)
    return MutableImport.concrete(
      module,
      undefined,
      multiSegmentAppSegment('from', path_),
      multiSegmentAppSegment('import', name_),
      undefined,
      undefined,
      undefined,
    )
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const segment = (segment: RawMultiSegmentAppSegment | undefined) => {
      const parts = []
      if (segment) parts.push(segment.header)
      if (segment?.body) parts.push(segment.body)
      return parts
    }
    const { polyglot, from, import: import_, all, as, hiding } = getAll(this.fields)
    yield* segment(polyglot)
    yield* segment(from)
    yield* segment(import_)
    if (all) yield all
    yield* segment(as)
    yield* segment(hiding)
  }
}

export class MutableImport extends Import implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & ImportFields>

  private toRaw(msas: OwnedMultiSegmentAppSegment): RawMultiSegmentAppSegment
  private toRaw(
    msas: OwnedMultiSegmentAppSegment | undefined,
  ): RawMultiSegmentAppSegment | undefined
  private toRaw(
    msas: OwnedMultiSegmentAppSegment | undefined,
  ): RawMultiSegmentAppSegment | undefined {
    return multiSegmentAppSegmentToRaw(this.module, msas, this.id)
  }

  setPolyglot<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set(
      'polyglot',
      value ? this.toRaw(multiSegmentAppSegment('polyglot', value)) : undefined,
    )
  }
  setFrom<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('from', value ? this.toRaw(multiSegmentAppSegment('from', value)) : value)
  }
  setImport<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('import', this.toRaw(multiSegmentAppSegment('import', value)))
  }
  setAll(value: Token | undefined) {
    this.fields.set('all', spaced(value))
  }
  setAs<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('as', this.toRaw(multiSegmentAppSegment('as', value)))
  }
  setHiding<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('hiding', this.toRaw(multiSegmentAppSegment('hiding', value)))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const { polyglot, from, import: import_, as, hiding } = getAll(this.fields)
    ;(polyglot?.body?.node === target
      ? this.setPolyglot
      : from?.body?.node === target
      ? this.setFrom
      : import_.body?.node === target
      ? this.setImport
      : as?.body?.node === target
      ? this.setAs
      : hiding?.body?.node === target
      ? this.setHiding
      : bail(`Failed to find child ${target} in node ${this.externalId}.`))(replacement)
  }
}
export interface MutableImport extends Import, MutableAst {
  get polyglot(): MutableAst | undefined
  get from(): MutableAst | undefined
  get import_(): MutableAst | undefined
  get as(): MutableAst | undefined
  get hiding(): MutableAst | undefined
}
applyMixins(MutableImport, [MutableAst])

type TextLiteralFields = {
  open: NodeChild<SyncTokenId> | undefined
  newline: NodeChild<SyncTokenId> | undefined
  elements: NodeChild[]
  close: NodeChild<SyncTokenId> | undefined
}
export class TextLiteral extends Ast {
  declare fields: FixedMapView<AstFields & TextLiteralFields>
  constructor(module: Module, fields: FixedMapView<AstFields & TextLiteralFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    newline: NodeChild<Token> | undefined,
    elements: NodeChild<Owned | Token>[],
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('TextLiteral')
    const id_ = base.get('id')
    const fields = setAll(base, {
      open,
      newline,
      elements: elements.map((elem) => concreteChild(module, elem, id_)),
      close,
    })
    return asOwned(new MutableTextLiteral(module, fields))
  }

  static new(rawText: string, module: MutableModule) {
    const open = unspaced(Token.new("'"))
    const elements = [unspaced(Token.new(escape(rawText)))]
    const close = unspaced(Token.new("'"))
    return this.concrete(module, open, undefined, elements, close)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, newline, elements, close } = getAll(this.fields)
    if (open) yield open
    if (newline) yield newline
    yield* elements
    if (close) yield close
  }
}

const mapping: Record<string, string> = {
  '\b': '\\b',
  '\f': '\\f',
  '\n': '\\n',
  '\r': '\\r',
  '\t': '\\t',
  '\v': '\\v',
  '"': '\\"',
  "'": "\\'",
  '`': '``',
}

/** Escape a string so it can be safely spliced into an interpolated (`''`) Enso string.
 * NOT USABLE to insert into raw strings. Does not include quotes. */
function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v"'`]/g, (match) => mapping[match]!)
}

export class MutableTextLiteral extends TextLiteral implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & TextLiteralFields>

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const replacement_ = autospaced(this.claimChild(replacement))
    this.fields.set(
      'elements',
      this.fields.get('elements').map((child) => (child.node === target ? replacement_ : child)),
    )
  }
}
export interface MutableTextLiteral extends TextLiteral, MutableAst {}
applyMixins(MutableTextLiteral, [MutableAst])

type DocumentedFields = {
  open: NodeChild<SyncTokenId> | undefined
  elements: NodeChild[]
  newlines: NodeChild<SyncTokenId>[]
  expression: NodeChild<AstId> | undefined
}
export class Documented extends Ast {
  declare fields: FixedMapView<AstFields & DocumentedFields>
  constructor(module: Module, fields: FixedMapView<AstFields & DocumentedFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    elements: NodeChild<Owned | Token>[],
    newlines: NodeChild<Token>[],
    expression: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('Documented')
    const id_ = base.get('id')
    const fields = setAll(base, {
      open,
      elements: elements.map((elem) => concreteChild(module, elem, id_)),
      newlines,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableDocumented(module, fields))
  }

  get expression(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('expression')?.node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, elements, newlines, expression } = getAll(this.fields)
    if (open) yield open
    yield* elements
    yield* newlines
    if (expression) yield expression
  }
}

export class MutableDocumented extends Documented implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & DocumentedFields>

  setExpression<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('expression')?.node === target) {
      this.setExpression(replacement)
    } else {
      const replacement_ = unspaced(this.claimChild(replacement))
      this.fields.set(
        'elements',
        this.fields.get('elements').map((child) => (child.node === target ? replacement_ : child)),
      )
    }
  }
}
export interface MutableDocumented extends Documented, MutableAst {
  get expression(): MutableAst | undefined
}
applyMixins(MutableDocumented, [MutableAst])

type InvalidFields = { expression: NodeChild<AstId> }
export class Invalid extends Ast {
  declare fields: FixedMapView<AstFields & InvalidFields>
  constructor(module: Module, fields: FixedMapView<AstFields & InvalidFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, expression: NodeChild<Owned>) {
    const base = module.baseObject('Invalid')
    return asOwned(new MutableInvalid(module, invalidFields(module, base, expression)))
  }

  get expression(): Ast {
    return this.module.checkedGet(this.fields.get('expression').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('expression')
  }
}

function invalidFields(
  module: MutableModule,
  base: FixedMap<AstFields>,
  expression: NodeChild<Owned>,
): FixedMap<AstFields & InvalidFields> {
  const id_ = base.get('id')
  return setAll(base, { expression: concreteChild(module, expression, id_) })
}

export class MutableInvalid extends Invalid implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & InvalidFields>

  /** Private, because it makes more sense to `.replace` the `Invalid` node. */
  private setExpression<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    assertEqual(this.fields.get('expression').node, target)
    this.setExpression(replacement)
  }
}
export interface MutableInvalid extends Invalid, MutableAst {
  /** The `expression` getter is intentionally not narrowed to provide mutable access:
   *  It makes more sense to `.replace` the `Invalid` node. */
}
applyMixins(MutableInvalid, [MutableAst])

type MutableRootPointer = MutableInvalid & { get expression(): MutableAst | undefined }
/** @internal */
export type RootPointer = Invalid

type GroupFields = {
  open: NodeChild<SyncTokenId> | undefined
  expression: NodeChild<AstId> | undefined
  close: NodeChild<SyncTokenId> | undefined
}
export class Group extends Ast {
  declare fields: FixedMapView<AstFields & GroupFields>
  constructor(module: Module, fields: FixedMapView<AstFields & GroupFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<Owned> | undefined,
    close: NodeChild<Token> | undefined,
  ) {
    const base = module.baseObject('Group')
    const id_ = base.get('id')
    const fields = setAll(base, { open, expression: concreteChild(module, expression, id_), close })
    return asOwned(new MutableGroup(module, fields))
  }

  get expression(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('expression')?.node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, expression, close } = getAll(this.fields)
    if (open) yield open
    if (expression) yield expression
    if (close) yield close
  }
}

export class MutableGroup extends Group implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & GroupFields>

  setExpression<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    assertEqual(this.fields.get('expression')?.node, target)
    this.setExpression(replacement)
  }
}
export interface MutableGroup extends Group, MutableAst {
  get expression(): MutableAst | undefined
}
applyMixins(MutableGroup, [MutableAst])

type NumericLiteralFields = {
  tokens: NodeChild<SyncTokenId>[]
}
export class NumericLiteral extends Ast {
  declare fields: FixedMapView<AstFields & NumericLiteralFields>
  constructor(module: Module, fields: FixedMapView<AstFields & NumericLiteralFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, tokens: NodeChild<Token>[]) {
    const base = module.baseObject('NumericLiteral')
    const fields = setAll(base, { tokens })
    return asOwned(new MutableNumericLiteral(module, fields))
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.fields.get('tokens')[Symbol.iterator]()
  }
}

export class MutableNumericLiteral extends NumericLiteral implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NumericLiteralFields>

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {}
}
export interface MutableNumericLiteral extends NumericLiteral, MutableAst {}
applyMixins(MutableNumericLiteral, [MutableAst])

/** The actual contents of an `ArgumentDefinition` are complex, but probably of more interest to the compiler than the
 *  GUI. We just need to represent them faithfully and create the simple cases. */
type ArgumentDefinition = NodeChild<Ast | Token>[]
type RawArgumentDefinition = NodeChild[]
type OwnedArgumentDefinition = NodeChild<Owned | Token>[]

function argumentDefinitionsToRaw(
  module: MutableModule,
  defs: OwnedArgumentDefinition[],
  parent: AstId,
): RawArgumentDefinition[] {
  return defs.map((def) =>
    def.map((part) => ({
      ...part,
      node: part.node instanceof Token ? part.node : claimChild(module, part.node, parent),
    })),
  )
}

type FunctionFields = {
  name: NodeChild<AstId>
  argumentDefinitions: RawArgumentDefinition[]
  equals: NodeChild<SyncTokenId>
  body: NodeChild<AstId> | undefined
}
export class Function extends Ast {
  declare fields: FixedMapView<AstFields & FunctionFields>
  constructor(module: Module, fields: FixedMapView<AstFields & FunctionFields>) {
    super(module, fields)
  }

  get name(): Ast {
    return this.module.checkedGet(this.fields.get('name').node)
  }
  get body(): Ast | undefined {
    return this.module.checkedGet(this.fields.get('body')?.node)
  }
  get argumentDefinitions(): ArgumentDefinition[] {
    return this.fields.get('argumentDefinitions').map((raw) =>
      raw.map((part) => ({
        ...part,
        node: this.module.getAny(part.node),
      })),
    )
  }

  static concrete(
    module: MutableModule,
    name: NodeChild<Owned>,
    argumentDefinitions: OwnedArgumentDefinition[],
    equals: NodeChild<Token>,
    body: NodeChild<Owned> | undefined,
  ) {
    const base = module.baseObject('Function')
    const id_ = base.get('id')
    const fields = setAll(base, {
      name: concreteChild(module, name, id_),
      argumentDefinitions: argumentDefinitionsToRaw(module, argumentDefinitions, id_),
      equals,
      body: concreteChild(module, body, id_),
    })
    return asOwned(new MutableFunction(module, fields))
  }

  static new(
    module: MutableModule,
    name: IdentLike,
    argumentDefinitions: OwnedArgumentDefinition[],
    body: Owned,
  ): Owned<MutableFunction> {
    // Note that a function name may not be an operator if the function is not in the body of a type definition, but we
    // can't easily enforce that because we don't currently make a syntactic distinction between top-level functions and
    // type methods.
    return MutableFunction.concrete(
      module,
      unspaced(Ident.newAllowingOperators(module, name)),
      argumentDefinitions,
      spaced(makeEquals()),
      autospaced(body),
    )
  }

  /** Construct a function with simple (name-only) arguments and a body block. */
  static fromStatements(
    module: MutableModule,
    name: IdentLike,
    argumentNames: StrictIdentLike[],
    statements: Owned[],
    trailingNewline?: boolean,
  ): Owned<MutableFunction> {
    const statements_: OwnedBlockLine[] = statements.map((statement) => ({
      expression: unspaced(statement),
    }))
    if (trailingNewline) {
      statements_.push({ expression: undefined })
    }
    const argumentDefinitions = argumentNames.map((name) => [spaced(Ident.new(module, name))])
    const body = BodyBlock.new(statements_, module)
    return MutableFunction.new(module, name, argumentDefinitions, body)
  }

  *bodyExpressions(): IterableIterator<Ast> {
    const body = this.body
    if (body instanceof BodyBlock) {
      yield* body.statements()
    } else if (body) {
      yield body
    }
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { name, argumentDefinitions, equals, body } = getAll(this.fields)
    yield name
    for (const def of argumentDefinitions) yield* def
    yield { whitespace: equals.whitespace ?? ' ', node: this.module.getToken(equals.node) }
    if (body) yield body
  }
}

export class MutableFunction extends Function implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & FunctionFields>

  setName<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('name', unspaced(this.claimChild(value)))
  }
  setBody<T extends MutableAst>(value: Owned<T> | undefined) {
    this.fields.set('body', unspaced(this.claimChild(value)))
  }
  setArgumentDefinitions(defs: OwnedArgumentDefinition[]) {
    this.fields.set('argumentDefinitions', argumentDefinitionsToRaw(this.module, defs, this.id))
  }

  /** Returns the body, after converting it to a block if it was empty or an inline expression. */
  bodyAsBlock(): MutableBodyBlock {
    const oldBody = this.body
    if (oldBody instanceof MutableBodyBlock) return oldBody
    const newBody = BodyBlock.new([], this.module)
    if (oldBody) newBody.push(oldBody.take())
    return newBody
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const { name, argumentDefinitions, body } = getAll(this.fields)
    if (name.node === target) {
      this.setName(replacement)
    } else if (body?.node === target) {
      this.setBody(replacement)
    } else {
      const replacement_ = this.claimChild(replacement)
      const replaceChild = (child: NodeChild) =>
        child.node === target ? { ...child, node: replacement_ } : child
      this.fields.set(
        'argumentDefinitions',
        argumentDefinitions.map((def) => def.map(replaceChild)),
      )
    }
  }
}
export interface MutableFunction extends Function, MutableAst {
  get name(): MutableAst
  get body(): MutableAst | undefined
}
applyMixins(MutableFunction, [MutableAst])

type AssignmentFields = {
  pattern: NodeChild<AstId>
  equals: NodeChild<SyncTokenId>
  expression: NodeChild<AstId>
}
export class Assignment extends Ast {
  declare fields: FixedMapView<AstFields & AssignmentFields>
  constructor(module: Module, fields: FixedMapView<AstFields & AssignmentFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    pattern: NodeChild<Owned>,
    equals: NodeChild<Token>,
    expression: NodeChild<Owned>,
  ) {
    const base = module.baseObject('Assignment')
    const id_ = base.get('id')
    const fields = setAll(base, {
      pattern: concreteChild(module, pattern, id_),
      equals,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableAssignment(module, fields))
  }

  static new(module: MutableModule, ident: StrictIdentLike, expression: Owned) {
    return Assignment.concrete(
      module,
      unspaced(Ident.new(module, ident)),
      spaced(makeEquals()),
      spaced(expression),
    )
  }

  get pattern(): Ast {
    return this.module.checkedGet(this.fields.get('pattern').node)
  }
  get expression(): Ast {
    return this.module.checkedGet(this.fields.get('expression').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { pattern, equals, expression } = getAll(this.fields)
    yield pattern
    yield {
      whitespace: equals.whitespace ?? expression.whitespace ?? ' ',
      node: equals.node,
    }
    yield expression
  }
}

export class MutableAssignment extends Assignment implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & AssignmentFields>

  setPattern<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('pattern', unspaced(this.claimChild(value)))
  }
  setExpression<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'expression', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const { pattern, expression } = getAll(this.fields)
    if (pattern.node === target) {
      this.setPattern(replacement)
    } else if (expression.node === target) {
      this.setExpression(replacement)
    }
  }
}
export interface MutableAssignment extends Assignment, MutableAst {
  get pattern(): MutableAst
  get expression(): MutableAst
}
applyMixins(MutableAssignment, [MutableAst])

type BodyBlockFields = {
  lines: RawBlockLine[]
}
export class BodyBlock extends Ast {
  declare fields: FixedMapView<AstFields & BodyBlockFields>
  constructor(module: Module, fields: FixedMapView<AstFields & BodyBlockFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, lines: OwnedBlockLine[]) {
    const base = module.baseObject('BodyBlock')
    const id_ = base.get('id')
    const fields = setAll(base, {
      lines: lines.map((line) => lineToRaw(line, module, id_)),
    })
    return asOwned(new MutableBodyBlock(module, fields))
  }

  static new(lines: OwnedBlockLine[], module: MutableModule) {
    return BodyBlock.concrete(module, lines)
  }

  get lines(): BlockLine[] {
    return this.fields.get('lines').map((line) => lineFromRaw(line, this.module))
  }

  *statements(): IterableIterator<Ast> {
    for (const line of this.lines) {
      if (line.expression) yield line.expression.node
    }
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    for (const line of this.fields.get('lines')) {
      yield line.newline ?? { node: Token.new('\n', RawAst.Token.Type.Newline) }
      if (line.expression) yield line.expression
    }
  }

  printSubtree(info: SpanMap, offset: number, parentIndent: string | undefined): string {
    let blockIndent: string | undefined
    let code = ''
    for (const line of this.fields.get('lines')) {
      code += line.newline.whitespace ?? ''
      const newlineCode = this.module.getToken(line.newline.node).code()
      // Only print a newline if this isn't the first line in the output, or it's a comment.
      if (offset || code || newlineCode.startsWith('#')) {
        // If this isn't the first line in the output, but there is a concrete newline token:
        // if it's a zero-length newline, ignore it and print a normal newline.
        code += newlineCode || '\n'
      }
      if (line.expression) {
        if (blockIndent === undefined) {
          if ((line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)) {
            blockIndent = line.expression.whitespace!
          } else if (parentIndent !== undefined) {
            blockIndent = parentIndent + '    '
          } else {
            blockIndent = ''
          }
        }
        const validIndent = (line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)
        code += validIndent ? line.expression.whitespace : blockIndent
        const lineNode = this.module.checkedGet(line.expression.node)
        assertEqual(lineNode.id, line.expression.node)
        assertEqual(parentId(lineNode), this.id)
        code += lineNode.printSubtree(info, offset + code.length, blockIndent)
      }
    }
    const span = nodeKey(offset, code.length)
    map.setIfUndefined(info.nodes, span, (): Ast[] => []).push(this)
    return code
  }
}

export class MutableBodyBlock extends BodyBlock implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & BodyBlockFields>

  updateLines(map: (lines: OwnedBlockLine[]) => OwnedBlockLine[]) {
    return this.setLines(map(this.takeLines()))
  }
  takeLines(): OwnedBlockLine[] {
    return this.fields.get('lines').map((line) => ownedLineFromRaw(line, this.module))
  }
  setLines(lines: OwnedBlockLine[]) {
    this.fields.set(
      'lines',
      lines.map((line) => lineToRaw(line, this.module, this.id)),
    )
  }

  /** Insert the given statement(s) starting at the specified line index. */
  insert(index: number, ...statements: Owned[]) {
    const before = this.fields.get('lines').slice(0, index)
    const insertions = statements.map((statement) => ({
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement)),
    }))
    const after = this.fields.get('lines').slice(index)
    this.fields.set('lines', [...before, ...insertions, ...after])
  }

  push(statement: Owned) {
    const oldLines = this.fields.get('lines')
    const newLine = {
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement)),
    }
    this.fields.set('lines', [...oldLines, newLine])
  }

  filter(keep: (ast: MutableAst) => boolean) {
    const oldLines = this.fields.get('lines')
    const filteredLines = oldLines.filter((line) => {
      if (!line.expression) return true
      return keep(this.module.checkedGet(line.expression.node))
    })
    this.fields.set('lines', filteredLines)
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {
    const replacement_ = this.claimChild(replacement)
    const updateLine = (line: RawBlockLine) =>
      line.expression?.node === target
        ? { ...line, expression: { ...line.expression, node: replacement_ } }
        : line
    this.fields.set('lines', this.fields.get('lines').map(updateLine))
  }
}
export interface MutableBodyBlock extends BodyBlock, MutableAst {
  statements(): IterableIterator<MutableAst>
}
applyMixins(MutableBodyBlock, [MutableAst])

type RawLine<T> = {
  newline: NodeChild<SyncTokenId>
  expression: NodeChild<T> | undefined
}
type Line<T> = {
  newline?: NodeChild<Token> | undefined
  expression: NodeChild<T> | undefined
}

type RawBlockLine = RawLine<AstId>
export type BlockLine = Line<Ast>
export type OwnedBlockLine = Line<Owned>

function lineFromRaw(raw: RawBlockLine, module: Module): BlockLine {
  const expression = raw.expression ? module.checkedGet(raw.expression.node) : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression: expression
      ? {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function ownedLineFromRaw(raw: RawBlockLine, module: MutableModule): OwnedBlockLine {
  const expression = raw.expression
    ? module.checkedGet(raw.expression.node).takeIfParented()
    : undefined
  return {
    newline: { ...raw.newline, node: module.getToken(raw.newline.node) },
    expression: expression
      ? {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : undefined,
  }
}

function lineToRaw(line: OwnedBlockLine, module: MutableModule, block: AstId): RawBlockLine {
  return {
    newline: line.newline ?? unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
    expression: line.expression
      ? {
          whitespace: line.expression?.whitespace,
          node: claimChild(module, line.expression.node, block),
        }
      : undefined,
  }
}

type IdentFields = {
  token: NodeChild<SyncTokenId>
}
export class Ident extends Ast {
  declare fields: FixedMapView<AstFields & IdentFields>
  constructor(module: Module, fields: FixedMapView<AstFields & IdentFields>) {
    super(module, fields)
  }

  get token(): IdentifierToken {
    return this.module.getToken(this.fields.get('token').node) as IdentifierToken
  }

  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Ident')
    const fields = setAll(base, { token })
    return asOwned(new MutableIdent(module, fields))
  }

  static new(module: MutableModule, ident: StrictIdentLike) {
    return Ident.concrete(module, unspaced(toIdentStrict(ident)))
  }

  /** @internal */
  static newAllowingOperators(module: MutableModule, ident: IdentLike) {
    return Ident.concrete(module, unspaced(toIdent(ident)))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }

  code(): Identifier {
    return this.token.code() as Identifier
  }
}
export class MutableIdent extends Ident implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & IdentFields>

  setToken(ident: IdentLike) {
    this.fields.set('token', unspaced(toIdent(ident)))
  }

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {}

  code(): Identifier {
    return this.token.code()
  }
}
export interface MutableIdent extends Ident, MutableAst {}
applyMixins(MutableIdent, [MutableAst])

type WildcardFields = {
  token: NodeChild<SyncTokenId>
}
export class Wildcard extends Ast {
  declare fields: FixedMapView<AstFields & WildcardFields>
  constructor(module: Module, fields: FixedMapView<AstFields & WildcardFields>) {
    super(module, fields)
  }

  get token(): Token {
    return this.module.getToken(this.fields.get('token').node)
  }

  static concrete(module: MutableModule, token: NodeChild<Token>) {
    const base = module.baseObject('Wildcard')
    const fields = setAll(base, { token })
    return asOwned(new MutableWildcard(module, fields))
  }

  static new(module: MutableModule) {
    const token = Token.new('_', RawAst.Token.Type.Wildcard)
    return this.concrete(module, unspaced(token))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }
}

export class MutableWildcard extends Wildcard implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & WildcardFields>

  replaceChild<T extends MutableAst>(target: AstId, replacement: Owned<T>) {}
}
export interface MutableWildcard extends Wildcard, MutableAst {}
applyMixins(MutableWildcard, [MutableAst])

export function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
): { root: Owned; spans: SpanMap; toRaw: Map<AstId, RawAst.Tree> } {
  const tokens = new Map()
  const nodes = new Map()
  const toRaw = new Map()
  const root = abstractTree(module, tree, code, nodes, tokens, toRaw).node
  const spans = { tokens, nodes }
  return { root, spans, toRaw }
}

function abstractTree(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  nodesOut: NodeSpanMap,
  tokensOut: TokenSpanMap,
  toRaw: Map<AstId, RawAst.Tree>,
): { whitespace: string | undefined; node: Owned } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesOut, tokensOut, toRaw)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokensOut)
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild<Owned | Token>[] = []
    const visitor = (child: LazyObject) => {
      if (RawAst.Tree.isInstance(child)) {
        children.push(recurseTree(child))
      } else if (RawAst.Token.isInstance(child)) {
        children.push(recurseToken(child))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }
  const whitespaceStart = tree.whitespaceStartInCodeParsed
  const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  const spanKey = nodeKey(codeStart, codeEnd - codeStart)
  let node: Owned
  switch (tree.type) {
    case RawAst.Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = recurseToken(line.newline)
        const expression = line.expression ? recurseTree(line.expression) : undefined
        return { newline, expression }
      })
      node = BodyBlock.concrete(module, lines)
      break
    }
    case RawAst.Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const argumentDefinitions = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body = tree.body !== undefined ? recurseTree(tree.body) : undefined
      node = Function.concrete(module, name, argumentDefinitions, equals, body)
      break
    }
    case RawAst.Tree.Type.Ident: {
      const token = recurseToken(tree.token)
      node = Ident.concrete(module, token)
      break
    }
    case RawAst.Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      node = Assignment.concrete(module, pattern, equals, value)
      break
    }
    case RawAst.Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      node = App.concrete(module, func, undefined, undefined, arg)
      break
    }
    case RawAst.Tree.Type.NamedApp: {
      const func = recurseTree(tree.func)
      const open = tree.open ? recurseToken(tree.open) : undefined
      const name = recurseToken(tree.name)
      const equals = recurseToken(tree.equals)
      const arg = recurseTree(tree.arg)
      const close = tree.close ? recurseToken(tree.close) : undefined
      const parens = open && close ? { open, close } : undefined
      const nameSpecification = { name, equals }
      node = App.concrete(module, func, parens, nameSpecification, arg)
      break
    }
    case RawAst.Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : undefined
      if (arg && opr.node.code() === '-') {
        node = NegationApp.concrete(module, opr, arg)
      } else {
        node = UnaryOprApp.concrete(module, opr, arg)
      }
      break
    }
    case RawAst.Tree.Type.OprApp: {
      const lhs = tree.lhs ? recurseTree(tree.lhs) : undefined
      const opr = tree.opr.ok
        ? [recurseToken(tree.opr.value)]
        : Array.from(tree.opr.error.payload.operators, recurseToken)
      const rhs = tree.rhs ? recurseTree(tree.rhs) : undefined
      if (opr.length === 1 && opr[0]?.node.code() === '.' && rhs?.node instanceof MutableIdent) {
        // Propagate type.
        const rhs_ = { ...rhs, node: rhs.node }
        node = PropertyAccess.concrete(module, lhs, opr[0], rhs_)
      } else {
        node = OprApp.concrete(module, lhs, opr, rhs)
      }
      break
    }
    case RawAst.Tree.Type.Number: {
      const tokens = []
      if (tree.base) tokens.push(recurseToken(tree.base))
      if (tree.integer) tokens.push(recurseToken(tree.integer))
      if (tree.fractionalDigits) {
        tokens.push(recurseToken(tree.fractionalDigits.dot))
        tokens.push(recurseToken(tree.fractionalDigits.digits))
      }
      node = NumericLiteral.concrete(module, tokens)
      break
    }
    case RawAst.Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      node = Wildcard.concrete(module, token)
      break
    }
    // These expression types are (or will be) used for backend analysis.
    // The frontend can ignore them, avoiding some problems with expressions sharing spans
    // (which makes it impossible to give them unique IDs in the current IdMap format).
    case RawAst.Tree.Type.OprSectionBoundary:
    case RawAst.Tree.Type.TemplateFunction:
      return { whitespace, node: recurseTree(tree.ast).node }
    case RawAst.Tree.Type.Invalid: {
      const expression = recurseTree(tree.ast)
      node = Invalid.concrete(module, expression)
      break
    }
    case RawAst.Tree.Type.Group: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const expression = tree.body ? recurseTree(tree.body) : undefined
      const close = tree.close ? recurseToken(tree.close) : undefined
      node = Group.concrete(module, open, expression, close)
      break
    }
    case RawAst.Tree.Type.TextLiteral: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const newline = tree.newline ? recurseToken(tree.newline) : undefined
      const elements = []
      for (const e of tree.elements) {
        elements.push(...visitChildren(e))
      }
      const close = tree.close ? recurseToken(tree.close) : undefined
      node = TextLiteral.concrete(module, open, newline, elements, close)
      break
    }
    case RawAst.Tree.Type.Documented: {
      const open = recurseToken(tree.documentation.open)
      const elements = []
      for (const e of tree.documentation.elements) {
        elements.push(...visitChildren(e))
      }
      const newlines = Array.from(tree.documentation.newlines, recurseToken)
      const expression = tree.expression ? recurseTree(tree.expression) : undefined
      node = Documented.concrete(module, open, elements, newlines, expression)
      break
    }
    case RawAst.Tree.Type.Import: {
      const recurseBody = (tree: RawAst.Tree) => {
        const body = recurseTree(tree)
        if (body.node instanceof Invalid && body.node.code() === '') return undefined
        return body
      }
      const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
        header: recurseToken(segment.header),
        body: segment.body ? recurseBody(segment.body) : undefined,
      })
      const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : undefined
      const from = tree.from ? recurseSegment(tree.from) : undefined
      const import_ = recurseSegment(tree.import)
      const all = tree.all ? recurseToken(tree.all) : undefined
      const as = tree.as ? recurseSegment(tree.as) : undefined
      const hiding = tree.hiding ? recurseSegment(tree.hiding) : undefined
      node = Import.concrete(module, polyglot, from, import_, all, as, hiding)
      break
    }
    default: {
      node = Generic.concrete(module, visitChildren(tree))
    }
  }
  toRaw.set(node.id, tree)
  map.setIfUndefined(nodesOut, spanKey, (): Ast[] => []).push(node)
  return { node, whitespace }
}

function abstractToken(
  token: RawAst.Token,
  code: string,
  tokensOut: TokenSpanMap,
): { whitespace: string; node: Token } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const node = Token.new(tokenCode, token.type)
  tokensOut.set(key, node)
  return { whitespace, node }
}

declare const nodeKeyBrand: unique symbol
export type NodeKey = SourceRangeKey & { [nodeKeyBrand]: never }
declare const tokenKeyBrand: unique symbol
export type TokenKey = SourceRangeKey & { [tokenKeyBrand]: never }
function nodeKey(start: number, length: number): NodeKey {
  return sourceRangeKey([start, start + length]) as NodeKey
}
function tokenKey(start: number, length: number): TokenKey {
  return sourceRangeKey([start, start + length]) as TokenKey
}

type NodeSpanMap = Map<NodeKey, Ast[]>
type TokenSpanMap = Map<TokenKey, Token>

interface SpanMap {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
}

interface PrintedSource {
  info: SpanMap
  code: string
}

export function spanMapToIdMap(spans: SpanMap): IdMap {
  const idMap = new IdMap()
  for (const [key, token] of spans.tokens.entries()) {
    assert(isUuid(token.id))
    idMap.insertKnownId(sourceRangeFromKey(key), token.id)
  }
  for (const [key, asts] of spans.nodes.entries()) {
    for (const ast of asts) {
      assert(isUuid(ast.externalId))
      idMap.insertKnownId(sourceRangeFromKey(key), ast.externalId)
    }
  }
  return idMap
}

function spanMapToSpanGetter(spans: SpanMap): (id: AstId) => SourceRange | undefined {
  const reverseMap = new Map<AstId, SourceRange>()
  for (const [key, asts] of spans.nodes) {
    for (const ast of asts) {
      reverseMap.set(ast.id, sourceRangeFromKey(key))
    }
  }
  return (id) => reverseMap.get(id)
}

/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast): PrintedSource {
  const info: SpanMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = ast.printSubtree(info, 0, undefined)
  return { info, code }
}

/** Parse the input as a block. */
export function parseBlock(code: string, inModule?: MutableModule) {
  return parseBlockWithSpans(code, inModule).root
}

/** Parse the input. If it contains a single expression at the top level, return it; otherwise, return a block. */
export function parse(code: string, module?: MutableModule): Owned {
  const module_ = module ?? MutableModule.Transient()
  const ast = parseBlock(code, module_)
  const [expr] = ast.statements()
  if (!expr) return ast
  const parent = parentId(expr)
  if (parent) module_.delete(parent)
  expr.fields.set('parent', undefined)
  return asOwned(expr)
}

export function parseBlockWithSpans(
  code: string,
  inModule?: MutableModule,
): { root: Owned<MutableBodyBlock>; spans: SpanMap } {
  const tree = parseEnso(code)
  const module = inModule ?? MutableModule.Transient()
  return fromRaw(tree, code, module)
}

function fromRaw(
  tree: RawAst.Tree,
  code: string,
  inModule?: MutableModule,
): {
  root: Owned<MutableBodyBlock>
  spans: SpanMap
  toRaw: Map<AstId, RawAst.Tree>
} {
  const module = inModule ?? MutableModule.Transient()
  const ast = abstract(module, tree, code)
  const spans = ast.spans
  // The root of the tree produced by the parser is always a `BodyBlock`.
  const root = ast.root as Owned<MutableBodyBlock>
  return { root, spans, toRaw: ast.toRaw }
}

export function parseExtended(code: string, idMap?: IdMap | undefined, inModule?: MutableModule) {
  const rawRoot = parseEnso(code)
  const module = inModule ?? MutableModule.Transient()
  const { root, spans, toRaw, idMapUpdates } = module.ydoc.transact(() => {
    const { root, spans, toRaw } = fromRaw(rawRoot, code, module)
    root.module.replaceRoot(root)
    const idMapUpdates = idMap ? setExternalIds(root.module, spans, idMap) : 0
    return { root, spans, toRaw, idMapUpdates }
  })
  const getSpan = spanMapToSpanGetter(spans)
  const idMapOut = spanMapToIdMap(spans)
  return { root, idMap: idMapOut, getSpan, toRaw, idMapUpdates }
}

export function setExternalIds(module: MutableModule, spans: SpanMap, ids: IdMap) {
  let astsMatched = 0
  let idsUnmatched = 0
  let asts = 0
  module.root()?.visitRecursiveAst((_ast) => (asts += 1))
  for (const [key, externalId] of ids.entries()) {
    const asts = spans.nodes.get(key as NodeKey)
    if (asts) {
      for (const ast of asts) {
        astsMatched += 1
        module.getVersion(ast).setExternalId(externalId)
      }
    } else {
      idsUnmatched += 1
    }
  }
  if (DEBUG)
    console.info(
      `asts=${asts}, astsMatched=${astsMatched}, idsUnmatched=${idsUnmatched}, haveRoot=${!!module.root()}`,
    )
  return module.root() ? asts - astsMatched : 0
}

///////////////// UI extensions ////////////////////////

export class ReactiveModule implements Module {
  edit(): MutableModule {
    return this.ymodule.edit()
  }

  root(): Ast | undefined {
    const rootPointer = this.get(ROOT_ID)
    if (!rootPointer) return
    const rootPointer_ = rootPointer as RootPointer
    return rootPointer_.expression
  }

  /////////////////////////////////

  private readonly ymodule: MutableModule
  private readonly nodes: Map<AstId, FixedMapView<AstFields>>
  private readonly spans: Map<AstId, SourceRange>

  constructor(base: MutableModule) {
    this.ymodule = base
    this.nodes = reactive(new Map())
    this.spans = reactive(new Map())
    base.observe((update) => {
      for (const id of update.addNodes) this.nodes.set(id, new Map() as any)
      for (const id of update.deleteNodes) this.nodes.delete(id)
      for (const { id, fields } of update.updateNodes) {
        const node = this.nodes.get(id)
        assertDefined(node)
        for (const [key, value] of fields) {
          const node_ = node as unknown as Map<string, unknown>
          node_.set(key, value)
        }
      }
      this.rebuildSpans(update.deleteNodes)
    })
  }

  private rebuildSpans(deleted: AstId[]) {
    for (const id of deleted) this.spans.delete(id)
    const root = this.root()
    if (!root) return
    const printed = print(root)
    for (const [key, nodes] of printed.info.nodes) {
      const range = sourceRangeFromKey(key)
      for (const node of nodes) this.spans.set(node.fields.get('id'), range)
    }
  }

  getSpan(id: AstId): SourceRange | undefined {
    return this.spans.get(id)
  }

  checkedGet(id: AstId): Ast
  checkedGet(id: AstId | undefined): Ast | undefined {
    if (!id) return
    const ast = this.get(id)
    assertDefined(ast)
    return ast
  }

  get(id: AstId): Ast | undefined
  get(id: AstId | undefined): Ast | undefined {
    if (!id) return
    const fields = this.nodes.get(id)
    if (!fields) return
    return materialize(this, fields)
  }

  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return Token.withId(token.code_, token.tokenType_, token.id)
  }

  getAny(node: AstId | SyncTokenId): Ast | Token {
    return isTokenId(node) ? this.getToken(node) : this.checkedGet(node)
  }

  has(id: AstId): boolean {
    return this.nodes.has(id)
  }
}

function materialize(module: Module, fields: FixedMapView<AstFields>): Ast {
  const type = fields.get('type')
  const fields_ = fields as FixedMapView<any>
  switch (type) {
    case 'App':
      return new App(module, fields_)
    case 'UnaryOprApp':
      return new UnaryOprApp(module, fields_)
    case 'NegationApp':
      return new NegationApp(module, fields_)
    case 'OprApp':
      return new OprApp(module, fields_)
    case 'PropertyAccess':
      return new PropertyAccess(module, fields_)
    case 'Generic':
      return new Generic(module, fields_)
    case 'Import':
      return new Import(module, fields_)
    case 'TextLiteral':
      return new TextLiteral(module, fields_)
    case 'Documented':
      return new Documented(module, fields_)
    case 'Invalid':
      return new Invalid(module, fields_)
    case 'Group':
      return new Group(module, fields_)
    case 'NumericLiteral':
      return new NumericLiteral(module, fields_)
    case 'Function':
      return new Function(module, fields_)
    case 'Assignment':
      return new Assignment(module, fields_)
    case 'BodyBlock':
      return new BodyBlock(module, fields_)
    case 'Ident':
      return new Ident(module, fields_)
    case 'Wildcard':
      return new Wildcard(module, fields_)
  }
  bail(`Invalid type: ${type}`)
}

export function deserialize(serialized: string): Owned {
  const parsed: SerializedPrintedSource = JSON.parse(serialized)
  const nodes = new Map(unsafeEntries(parsed.info.nodes))
  const tokens = new Map(unsafeEntries(parsed.info.tokens))
  const module = MutableModule.Transient()
  const tree = parseEnso(parsed.code)
  const ast = abstract(module, tree, parsed.code)
  // TODO: ast <- nodes,tokens
  return ast.root
}

interface SerializedInfoMap {
  nodes: Record<NodeKey, AstId[]>
  tokens: Record<TokenKey, TokenId>
}

interface SerializedPrintedSource {
  info: SerializedInfoMap
  code: string
}

export function serialize(ast: Ast): string {
  return JSON.stringify(print(ast))
}

export type TokenTree = (TokenTree | string)[]
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (isTokenId(child.node)) {
      return module.getToken(child.node).code()
    } else {
      const node = module.get(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.externalId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (isTokenId(child.node)) {
        return module.getToken(child.node).code()
      } else {
        const node = module.get(child.node)
        return node ? tokenTreeWithIds(node) : ['<missing>']
      }
    }),
  ]
}

export function moduleMethodNames(topLevel: BodyBlock): Set<string> {
  const result = new Set<string>()
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function) {
      result.add(inner.name.code())
    }
  }
  return result
}

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(topLevel: BodyBlock, name: string): Function | undefined {
  for (const statement of topLevel.statements()) {
    const inner = statement.innerExpression()
    if (inner instanceof Function && inner.name.code() === name) {
      return inner
    }
  }
  return undefined
}

export function functionBlock(topLevel: BodyBlock, name: string) {
  const func = findModuleMethod(topLevel, name)
  if (!(func?.body instanceof BodyBlock)) return undefined
  return func.body
}

export function deleteFromParentBlock(ast: MutableAst) {
  const parent = ast.mutableParent()
  if (parent instanceof MutableBodyBlock)
    parent.updateLines((lines) => lines.filter((line) => line.expression?.node.id !== ast.id))
}

declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [TokenKey]: Token
  }
}
