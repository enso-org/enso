import * as RawAst from '@/generated/ast'
import { assert, assertDefined, assertEqual, bail } from '@/util/assert'
import { parseEnso } from '@/util/ast'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import { Err, Ok, type Result } from '@/util/data/result'
import type { LazyObject } from '@/util/parserSupport'
import { unsafeEntries } from '@/util/record'
import * as map from 'lib0/map'
import * as random from 'lib0/random'
import { reactive } from 'vue'
import * as Y from 'yjs'
import { IdMap, type ExprId, type SourceRange } from '../../../shared/yjsModel'

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

/*
export class MutableModule {
  apply(editIn: Module) {
    const edit = editIn.raw
    if (edit.spans) {
      const spans = this.spans ?? new Map()
      for (const [id, ast] of edit.spans.entries()) {
        spans.set(id, ast)
      }
      this.spans = spans
    }
    for (const [id, ast] of edit.nodes.entries()) {
      if (ast === null) {
        this.nodes.delete(id)
        this.spans?.delete(id)
      } else {
        this.nodes.set(id, ast)
      }
    }
  }

  // Replace the contents of this module with the contents of the specified module.
  replace(editIn: Module) {
    const edit = editIn.raw
    for (const id of this.nodes.keys()) {
      if (!edit.nodes.has(id)) {
        this.nodes.delete(id)
        this.spans?.delete(id)
      }
    }
    this.apply(edit)
  }
}
*/

export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn)
  const module = MutableModule.Transient()
  const tree = parseEnso(printed.code)
  return abstract(module, tree, printed.code, printed.info).node
}

export type NodeChild<T = SyncId | SyncTokenId> = { whitespace?: string | undefined; node: T }

declare const brandAstId: unique symbol
declare const brandTokenId: unique symbol
declare const brandSyncAstId: unique symbol
export type AstId = ExprId & { [brandAstId]: never }
export type TokenId = ExprId & { [brandTokenId]: never }
export type SyncId = string & { [brandSyncAstId]: never }

function newAstId(): AstId {
  return random.uuidv4() as AstId
}
function newTokenId(): TokenId {
  return random.uuidv4() as TokenId
}
function newSyncId(type: string): SyncId {
  return `${type}#${random.uint53()}` as SyncId
}
const rootId = `Root` as SyncId
// Cast an ID which may be a tree or a token to a tree ID.
export function asNodeId(expr: ExprId): AstId {
  return expr as AstId
}

interface SyncTokenId {
  readonly exprId: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined
}
export class Token implements SyncTokenId {
  readonly exprId: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined

  constructor(code: string, id: TokenId, type: RawAst.Token.Type | undefined) {
    this.exprId = id
    this.code_ = code
    this.tokenType_ = type
  }

  static new(code: string, type?: RawAst.Token.Type, id?: TokenId) {
    return new this(code, id ?? newTokenId(), type)
  }

  code(): string {
    return this.code_
  }

  typeName(): string {
    if (this.tokenType_) return RawAst.Token.typeNames[this.tokenType_]!
    else return 'Raw'
  }
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

/*
function makeChild<T extends MutableAst>(module: MutableModule, child: Owned<T>, parent: AstId): AstId {
  const oldParent = child.parentId
  assert(
    !oldParent || !module.has(oldParent),
    'Owned object is not owned. Was it obtained from a different module?',
  )
  const spliced = module.splice(child)
  spliced.parent = parent
  return spliced.exprId
}
 */

////////////////////////////////////////////////////////

export interface Module {
  edit(): MutableModule
  getAst(id: SyncId): Ast
  getAst(id: SyncId | undefined): Ast | undefined
  tryGetAst(id: SyncId): Ast | undefined
  tryGetAst(id: SyncId | undefined): Ast | undefined
  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined
  getAny(node: SyncId | SyncTokenId): MutableAst | Token
  has(id: SyncId): boolean
  //root(): Ast | undefined

  spans: Map<AstId, SourceRange> | undefined
}

export class MutableModule implements Module {
  private readonly nodes: Y.Map<Y.Map<unknown>>
  spans: Map<AstId, SourceRange> | undefined

  /** Return this module's copy of `ast`, if this module was created by cloning `ast`'s module. */
  get<T extends Ast>(ast: T): Mutable<T> | undefined {
    const instance = this.tryGetAst(syncId(ast))
    if (!instance) return
    return instance as Mutable<T>
  }
  getIfDefined<T extends Ast>(ast: T | undefined): Mutable<T> | undefined {
    if (ast) return this.get(ast)
  }

  edit(): MutableModule {
    const state = Y.encodeStateAsUpdateV2(this.nodes.doc!)
    const doc = new Y.Doc()
    Y.applyUpdateV2(doc, state)
    return new MutableModule(doc, undefined)
  }

  /** Copy the given node into the module. */
  copy<T extends Ast>(ast: T): Owned<Mutable<T>> {
    const id = newSyncId(typeName(ast))
    const fields = ast.fields.clone()
    this.nodes.set(id, fields as any)
    fields.set('id', id)
    fields.set('parent', undefined)
    const ast_ = materialize(this, fields)
    ast_.importReferences(ast.module)
    return ast_ as Owned<Mutable<typeof ast>>
  }

  static Transient() {
    return new this(new Y.Doc(), undefined)
  }

  static Observable() {
    // TODO
    //(reactive(new Map()), reactive(new Map()))
    return this.Transient()
  }

  /////////////////////////////////////////////

  constructor(doc: Y.Doc, spans: Map<AstId, SourceRange> | undefined) {
    this.nodes = doc.getMap<Y.Map<unknown>>('nodes')
    this.spans = spans
  }

  /** @internal */
  baseObject(type: string, exprId?: AstId): FixedMap<AstFields> {
    const map = new Y.Map<unknown>()
    const map_ = map as unknown as FixedMap<{}>
    const id = newSyncId(type)
    const fields = setAll(map_, {
      id,
      exprId: exprId ?? newAstId(),
      type: type,
      parent: undefined,
    })
    this.nodes.set(id, map)
    return fields
  }

  /** @internal */
  getAst(id: SyncId): MutableAst
  getAst(id: SyncId | undefined): MutableAst | undefined
  getAst(id: SyncId | undefined): MutableAst | undefined {
    if (!id) return undefined
    const ast = this.tryGetAst(id)
    assert(ast !== undefined, 'id in module')
    return ast
  }

  /** @internal */
  tryGetAst(id: SyncId): MutableAst | undefined
  tryGetAst(id: SyncId | undefined): MutableAst | undefined
  tryGetAst(id: SyncId | undefined): MutableAst | undefined {
    if (!id) return undefined
    const nodeData = this.nodes.get(id)
    if (!nodeData) return undefined
    const fields = nodeData as any
    return materialize(this, fields)
  }

  /** @internal */
  getToken(token: SyncTokenId): Token
  getToken(token: SyncTokenId | undefined): Token | undefined
  getToken(token: SyncTokenId | undefined): Token | undefined {
    if (!token) return token
    if (token instanceof Token) return token
    return Token.new(token.code_, token.tokenType_, token.exprId)
  }

  getAny(node: SyncId | SyncTokenId): MutableAst | Token {
    return isTokenId(node) ? this.getToken(node) : this.getAst(node)
  }

  /** @internal Copy a node into the module, if it is bound to a different module. */
  splice<T extends MutableAst>(ast: Owned<T>): Owned<T>
  splice<T extends MutableAst>(ast: Owned<T> | undefined): Owned<T> | undefined {
    if (!ast) return ast
    if (ast.module === this) return ast
    return this.copy(ast) as any
  }

  /** @internal */
  delete(id: SyncId) {
    this.nodes.delete(id)
  }

  /** @internal */
  has(id: SyncId) {
    return this.nodes.has(id)
  }
}

type Mutable<T extends Ast> = T extends App
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
  : T extends NegationOprApp
  ? MutableNegationOprApp
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

function materialize(module: MutableModule, fields: FixedMap<AstFields>): MutableAst {
  const type = fields.get('type')
  switch (type) {
    case 'App':
      return new MutableApp(module, fields)
    case 'UnaryOprApp':
      return new MutableUnaryOprApp(module, fields)
    case 'NegationOprApp':
      return new MutableNegationOprApp(module, fields)
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

type FixedMapView<Fields> = {
  get<Key extends string & keyof Fields>(key: Key): Fields[Key]
  entries(): IterableIterator<readonly [string, unknown]>
  clone(): FixedMap<Fields>
}

type FixedMap<Fields> = FixedMapView<Fields> & {
  set<Key extends string & keyof Fields>(key: Key, value: Fields[Key]): void
}

function getAll<Fields extends object>(map: FixedMap<Fields>): Fields {
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

type AstFields = {
  id: SyncId
  exprId: AstId
  type: string
  parent: SyncId | undefined
}
function parentId(ast: Ast): SyncId | undefined {
  return ast.fields.get('parent')
}
function syncId(ast: Ast): SyncId {
  return ast.fields.get('id')
}
function typeName(ast: Ast): string {
  return ast.fields.get('type')
}

function isTokenId(
  t: SyncTokenId | SyncId | Ast | Owned<Ast> | Owned<MutableAst>,
): t is SyncTokenId {
  return typeof t === 'object' && !(t instanceof Ast)
}

function isToken(t: unknown): t is Token {
  return t instanceof Token
}

export abstract class Ast {
  readonly module: Module
  /** @internal */
  readonly fields: FixedMapView<AstFields>

  get exprId(): AstId {
    return this.fields.get('exprId')
  }

  /**
   * Return whether `this` and `other` are the same object. If true, changes to either will be immediately visible in the other.
   *
   * NOTE: This method should be used in place of `===`. Distinct JS objects may be the same logical object.
   */
  is<T extends Ast>(other: T): boolean {
    return this.fields === other.fields
  }

  /** Return this node's span, if it belongs to a module with an associated span map. */
  get span(): SourceRange | undefined {
    return this.module.spans?.get(this.exprId)
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

  printSubtree(info: InfoMap, offset: number, parentIndent: string | undefined): string {
    let code = ''
    for (const child of this.concreteChildren()) {
      if (!isTokenId(child.node) && this.module.getAst(child.node) === undefined) continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        code += ' '
      }
      if (isTokenId(child.node)) {
        const tokenStart = offset + code.length
        const tokenCode = this.module.getToken(child.node).code()
        const span = tokenKey(tokenStart, tokenCode.length)
        info.tokens.set(span, child.node.exprId)
        code += tokenCode
      } else {
        const childNode = this.module.getAst(child.node)
        assert(childNode != null)
        code += childNode.printSubtree(info, offset + code.length, parentIndent)
        // Extra structural validation.
        assertEqual(syncId(childNode), child.node)
        if (parentId(childNode) !== this.syncId) {
          console.error(`Inconsistent parent pointer (expected ${this.syncId})`, childNode)
        }
        assertEqual(parentId(childNode), this.syncId)
      }
    }
    const span = nodeKey(offset, code.length)
    const infos = map.setIfUndefined(info.nodes, span, (): AstId[] => [])
    infos.unshift(this.exprId)
    return code
  }

  serialize(): string {
    return JSON.stringify(print(this))
  }

  static deserialize(serialized: string): Ast {
    const parsed: SerializedPrintedSource = JSON.parse(serialized)
    const nodes = new Map(unsafeEntries(parsed.info.nodes))
    const tokens = new Map(unsafeEntries(parsed.info.tokens))
    const tokensOut = new Map()
    const module = MutableModule.Transient()
    const tree = parseEnso(parsed.code)
    return abstract(module, tree, parsed.code, { nodes, tokens, tokensOut }).node
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Token> {
    for (const child of this.concreteChildren()) {
      if (isTokenId(child.node)) {
        yield this.module.getToken(child.node)
      } else {
        const node = this.module.getAst(child.node)
        if (node) yield node
      }
    }
  }

  static parseBlock(source: PrintedSource | string, inModule?: MutableModule) {
    return parseBlock(source, inModule)
  }

  static parse(source: PrintedSource | string, module?: MutableModule) {
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

  protected get syncId() {
    return syncId(this)
  }
}

export interface MutableAst {}
export abstract class MutableAst extends Ast {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields>

  /** Modify the parent of this node to refer to a new object instead. Return the object, which now has no parent. */
  replace<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const parent = this.module.getAst(parentId(this))
    assert(parent !== undefined)
    parent.replaceChild(this.syncId, replacement)
    this.fields.set('parent', undefined)
    return asOwned(this)
  }

  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends MutableAst>(replacement: Owned<T>): Owned<typeof this> {
    const old = this.replace(replacement)
    replacement.fields.set('exprId', old.exprId)
    old.fields.set('exprId', newAstId())
    return old
  }

  /** Replace the parent of this object with a reference to a new placeholder object. Return the object, now parentless. */
  take(): Removed<typeof this> {
    if (parentId(this)) {
      const placeholder = Wildcard.new(this.module)
      const node = this.replace(placeholder)
      return { node, placeholder }
    } else {
      return { node: asOwned(this), placeholder: undefined }
    }
  }

  takeIfParented<T extends MutableAst>(): Owned<typeof this> {
    const parent = parentId(this)
    if (parent) {
      const parentAst = this.module.getAst(parent)
      const placeholder = Wildcard.new(this.module)
      parentAst.replaceChild(this.syncId, placeholder)
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

  takeAndReplace<T extends MutableAst>(wrap: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.take()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = wrap(taken.node)
    taken.placeholder.replace(replacement)
    return replacement
  }

  takeAndReplaceValue<T extends MutableAst>(wrap: (x: Owned<typeof this>) => Owned<T>): T {
    const taken = this.takeValue()
    assertDefined(taken.placeholder, 'To replace an `Ast`, it must have a parent.')
    const replacement = wrap(taken.node)
    taken.placeholder.replaceValue(replacement)
    return replacement
  }

  ///////////////////

  /** @internal */
  importReferences(module: Module) {
    if (module === this.module) return
    for (const child of this.concreteChildren()) {
      if (!isTokenId(child.node)) {
        const childInForeignModule = module.getAst(child.node)
        assert(childInForeignModule !== undefined)
        const importedChild = this.module.copy(childInForeignModule)
        importedChild.fields.set('parent', undefined)
        this.replaceChild(child.node, asOwned(importedChild))
      }
    }
  }

  protected abstract replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>): void

  protected claimChild<T extends MutableAst>(child: Owned<T>): SyncId
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): SyncId | undefined
  protected claimChild<T extends MutableAst>(child: Owned<T> | undefined): SyncId | undefined {
    return child ? claimChild(this.module, child, this.syncId) : undefined
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
  parent: SyncId,
): SyncId {
  const child_ = module.splice(child)
  child_.fields.set('parent', parent)
  return syncId(child_)
}

function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned<MutableAst>>,
  parent: SyncId,
): NodeChild<SyncId>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned<MutableAst>> | undefined,
  parent: SyncId,
): NodeChild<SyncId> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned<MutableAst> | Token>,
  parent: SyncId,
): NodeChild<SyncId | Token>
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned<MutableAst> | Token> | undefined,
  parent: SyncId,
): NodeChild<SyncId | Token> | undefined
function concreteChild(
  module: MutableModule,
  child: NodeChild<Owned<MutableAst> | Token> | undefined,
  parent: SyncId,
): NodeChild<SyncId | Token> | undefined {
  if (!child) return undefined
  if (isTokenId(child.node)) return child as NodeChild<Token>
  return { ...child, node: claimChild(module, child.node, parent) }
}

function toIdent(ident: string | Token): Token
function toIdent(ident: string | Token | undefined): Token | undefined
function toIdent(ident: string | Token | undefined): Token | undefined {
  return ident ? (isToken(ident) ? ident : Token.new(ident, RawAst.Token.Type.Ident)) : undefined
}
function makeEquals(): Token {
  return Token.new('=', RawAst.Token.Type.Operator)
}

function nameSpecification(
  name: string | Token | undefined,
): { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined {
  if (name === undefined) return undefined
  return { name: autospaced(toIdent(name)), equals: unspaced(makeEquals()) }
}

type AppFields = {
  function: NodeChild<SyncId>
  parens: { open: NodeChild<SyncTokenId>; close: NodeChild<SyncTokenId> } | undefined
  nameSpecification: { name: NodeChild<SyncTokenId>; equals: NodeChild<SyncTokenId> } | undefined
  argument: NodeChild<SyncId>
}

export class App extends Ast {
  declare fields: FixedMap<AstFields & AppFields>
  constructor(module: Module, fields: FixedMap<AstFields & AppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    func: NodeChild<Owned<MutableAst>>,
    parens: { open: NodeChild<Token>; close: NodeChild<Token> } | undefined,
    nameSpecification: { name: NodeChild<Token>; equals: NodeChild<Token> } | undefined,
    argument: NodeChild<Owned<MutableAst>>,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('App', id)
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
    func: Owned<MutableAst>,
    argumentName: string | Token | undefined,
    argument: Owned<MutableAst>,
  ) {
    return App.concrete(
      module,
      unspaced(func),
      undefined,
      nameSpecification(argumentName),
      autospaced(argument),
    )
  }

  get function(): Ast {
    return this.module.getAst(this.fields.get('function').node)
  }
  get argumentName(): Token | undefined {
    return this.module.getToken(this.fields.get('nameSpecification')?.name.node)
  }
  get argument(): Ast {
    return this.module.getAst(this.fields.get('argument').node)
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
function setNode<Fields, Key extends string & KeysOfFieldType<Fields, NodeChild<SyncId>>>(
  map: FixedMap<Fields>,
  key: Key,
  node: SyncId,
): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<SyncId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: SyncId | undefined): void
function setNode<
  Fields,
  Key extends string & KeysOfFieldType<Fields, NodeChild<SyncId> | undefined>,
>(map: FixedMap<Fields>, key: Key, node: SyncId | undefined): void {
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
  setArgumentName(name: string | Token | undefined) {
    this.fields.set('nameSpecification', nameSpecification(name))
  }
  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  argument: NodeChild<SyncId> | undefined
}
export class UnaryOprApp extends Ast {
  declare fields: FixedMap<AstFields & UnaryOprAppFields>
  constructor(module: Module, fields: FixedMap<AstFields & UnaryOprAppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned<MutableAst>> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('UnaryOprApp', id)
    const id_ = base.get('id')
    const fields = setAll(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableUnaryOprApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned<MutableAst> | undefined) {
    return this.concrete(module, unspaced(operator), argument ? autospaced(argument) : undefined)
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast | undefined {
    return this.module.getAst(this.fields.get('argument')?.node)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
    if (this.fields.get('argument')?.node === target) {
      this.setArgument(replacement)
    }
  }
}
export interface MutableUnaryOprApp extends UnaryOprApp, MutableAst {
  get argument(): MutableAst | undefined
}
applyMixins(MutableUnaryOprApp, [MutableAst])

type NegationOprAppFields = {
  operator: NodeChild<SyncTokenId>
  argument: NodeChild<SyncId>
}
export class NegationOprApp extends Ast {
  declare fields: FixedMap<AstFields & NegationOprAppFields>
  constructor(module: Module, fields: FixedMap<AstFields & NegationOprAppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    operator: NodeChild<Token>,
    argument: NodeChild<Owned<MutableAst>>,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('NegationOprApp', id)
    const id_ = base.get('id')
    const fields = setAll(base, {
      operator,
      argument: concreteChild(module, argument, id_),
    })
    return asOwned(new MutableNegationOprApp(module, fields))
  }

  static new(module: MutableModule, operator: Token, argument: Owned<MutableAst>) {
    return this.concrete(module, unspaced(operator), autospaced(argument))
  }

  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get argument(): Ast {
    return this.module.getAst(this.fields.get('argument').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }
}

export class MutableNegationOprApp extends NegationOprApp implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & NegationOprAppFields>

  setArgument<T extends MutableAst>(value: Owned<T>) {
    setNode(this.fields, 'argument', this.claimChild(value))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
    if (this.fields.get('argument')?.node === target) {
      this.setArgument(replacement)
    }
  }
}
export interface MutableNegationOprApp extends NegationOprApp, MutableAst {
  get argument(): MutableAst
}
applyMixins(MutableNegationOprApp, [MutableAst])

type OprAppFields = {
  lhs: NodeChild<SyncId> | undefined
  operators: NodeChild<SyncTokenId>[]
  rhs: NodeChild<SyncId> | undefined
}
export class OprApp extends Ast {
  declare fields: FixedMap<AstFields & OprAppFields>
  constructor(module: Module, fields: FixedMap<AstFields & OprAppFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned<MutableAst>> | undefined,
    operators: NodeChild<Token>[],
    rhs: NodeChild<Owned<MutableAst>> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('OprApp', id)
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
    lhs: Owned<MutableAst> | undefined,
    operator: Token,
    rhs: Owned<MutableAst> | undefined,
  ) {
    return OprApp.concrete(module, unspaced(lhs), [autospaced(operator)], autospaced(rhs))
  }

  get lhs(): Ast | undefined {
    return this.module.getAst(this.fields.get('lhs')?.node)
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
    return this.module.getAst(this.fields.get('rhs')?.node)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  lhs: NodeChild<SyncId> | undefined
  operator: NodeChild<SyncTokenId>
  rhs: NodeChild<SyncId>
}
export class PropertyAccess extends Ast {
  declare fields: FixedMap<AstFields & PropertyAccessFields>
  constructor(module: Module, fields: FixedMap<AstFields & PropertyAccessFields>) {
    super(module, fields)
  }

  static new(module: MutableModule, lhs: Owned<MutableAst>, rhs: Token | string) {
    const dot = unspaced(new Token('.', newTokenId(), RawAst.Token.Type.Operator))
    return this.concrete(module, unspaced(lhs), dot, unspaced(Ident.new(module, toIdent(rhs))))
  }

  static Sequence(
    segments: [Token | string, ...(Token | string)[]],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent>
  static Sequence(
    segments: (Token | string)[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined
  static Sequence(
    segments: (Token | string)[],
    module: MutableModule,
  ): Owned<MutablePropertyAccess> | Owned<MutableIdent> | undefined {
    let path
    for (const s of segments) {
      const t = toIdent(s)
      path = path ? this.new(module, path, t) : Ident.new(module, t)
    }
    return path
  }

  static concrete(
    module: MutableModule,
    lhs: NodeChild<Owned<MutableAst>> | undefined,
    operator: NodeChild<Token>,
    rhs: NodeChild<Owned<MutableIdent>>,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('PropertyAccess', id)
    const id_ = base.get('id')
    const fields = setAll(base, {
      lhs: concreteChild(module, lhs, id_),
      operator,
      rhs: concreteChild(module, rhs, id_),
    })
    return asOwned(new MutablePropertyAccess(module, fields))
  }

  get lhs(): Ast | undefined {
    return this.module.getAst(this.fields.get('lhs')?.node)
  }
  get operator(): Token {
    return this.module.getToken(this.fields.get('operator').node)
  }
  get rhs(): Token {
    const ast = this.module.getAst(this.fields.get('rhs').node)
    assert(ast instanceof Ident)
    return ast.token
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
  setRhs(ident: Token) {
    const node = this.claimChild(Ident.new(this.module, ident))
    const old = this.fields.get('rhs')
    this.fields.set('rhs', old ? { ...old, node } : unspaced(node))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  declare fields: FixedMap<AstFields & GenericFields>
  constructor(module: Module, fields: FixedMap<AstFields & GenericFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    children: NodeChild<Owned<MutableAst> | Token>[],
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Generic', id)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  body: NodeChild<SyncId> | undefined
}
type OwnedMultiSegmentAppSegment = {
  header: NodeChild<Token>
  body: NodeChild<Owned<MutableAst>> | undefined
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
    header: { node: new Token(header, newTokenId(), RawAst.Token.Type.Ident) },
    body: spaced(body ? (body as any) : undefined),
  }
}

function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: SyncId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment,
  parent: SyncId,
): RawMultiSegmentAppSegment
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: SyncId,
): RawMultiSegmentAppSegment | undefined
function multiSegmentAppSegmentToRaw(
  module: MutableModule,
  msas: OwnedMultiSegmentAppSegment | undefined,
  parent: SyncId,
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
  declare fields: FixedMap<AstFields & ImportFields>
  constructor(module: Module, fields: FixedMap<AstFields & ImportFields>) {
    super(module, fields)
  }

  get polyglot(): Ast | undefined {
    return this.module.getAst(this.fields.get('polyglot')?.body?.node)
  }
  get from(): Ast | undefined {
    return this.module.getAst(this.fields.get('from')?.body?.node)
  }
  get import_(): Ast | undefined {
    return this.module.getAst(this.fields.get('import').body?.node)
  }
  get all(): Token | undefined {
    return this.module.getToken(this.fields.get('all')?.node)
  }
  get as(): Ast | undefined {
    return this.module.getAst(this.fields.get('as')?.body?.node)
  }
  get hiding(): Ast | undefined {
    return this.module.getAst(this.fields.get('hiding')?.body?.node)
  }

  static concrete(
    module: MutableModule,
    polyglot: OwnedMultiSegmentAppSegment | undefined,
    from: OwnedMultiSegmentAppSegment | undefined,
    import_: OwnedMultiSegmentAppSegment,
    all: NodeChild<Token> | undefined,
    as: OwnedMultiSegmentAppSegment | undefined,
    hiding: OwnedMultiSegmentAppSegment | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Import', id)
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

  static Qualified(path: string[], module: MutableModule): Owned<MutableImport> | undefined {
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
    path: (Token | string)[],
    name: Token | string,
    module: MutableModule,
  ): Owned<MutableImport> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const name_ = Ident.new(module, name)
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
    return multiSegmentAppSegmentToRaw(this.module, msas, this.syncId)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
      : bail(`Failed to find child ${target} in node ${this.exprId}.`))(replacement)
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
  declare fields: FixedMap<AstFields & TextLiteralFields>
  constructor(module: Module, fields: FixedMap<AstFields & TextLiteralFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    newline: NodeChild<Token> | undefined,
    elements: NodeChild<Owned<MutableAst> | Token>[],
    close: NodeChild<Token> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('TextLiteral', id)
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

export class MutableTextLiteral extends TextLiteral implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & TextLiteralFields>

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  expression: NodeChild<SyncId> | undefined
}
export class Documented extends Ast {
  declare fields: FixedMap<AstFields & DocumentedFields>
  constructor(module: Module, fields: FixedMap<AstFields & DocumentedFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    elements: NodeChild<Owned<MutableAst> | Token>[],
    newlines: NodeChild<Token>[],
    expression: NodeChild<Owned<MutableAst>> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Documented', id)
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
    return this.module.getAst(this.fields.get('expression')?.node)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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

type InvalidFields = { expression: NodeChild<SyncId> }
export class Invalid extends Ast {
  declare fields: FixedMap<AstFields & InvalidFields>
  constructor(module: Module, fields: FixedMap<AstFields & InvalidFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    expression: NodeChild<Owned<MutableAst>>,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Invalid', id)
    const id_ = base.get('id')
    const fields = setAll(base, { expression: concreteChild(module, expression, id_) })
    return asOwned(new MutableInvalid(module, fields))
  }

  get expression(): Ast {
    return this.module.getAst(this.fields.get('expression').node)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('expression')
  }
}

export class MutableInvalid extends Invalid implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & InvalidFields>

  /** Private, because it makes more sense to `.replace` the `Invalid` node. */
  private setExpression<T extends MutableAst>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
    assertEqual(this.fields.get('expression').node, target)
    this.setExpression(replacement)
  }
}
export interface MutableInvalid extends Invalid, MutableAst {
  /** The `expression` getter is intentionally not narrowed to provide mutable access:
   *  It makes more sense to `.replace` the `Invalid` node. */
}
applyMixins(MutableInvalid, [MutableAst])

type GroupFields = {
  open: NodeChild<SyncTokenId> | undefined
  expression: NodeChild<SyncId> | undefined
  close: NodeChild<SyncTokenId> | undefined
}
export class Group extends Ast {
  declare fields: FixedMap<AstFields & GroupFields>
  constructor(module: Module, fields: FixedMap<AstFields & GroupFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<Owned<MutableAst>> | undefined,
    close: NodeChild<Token> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Group', id)
    const id_ = base.get('id')
    const fields = setAll(base, { open, expression: concreteChild(module, expression, id_), close })
    return asOwned(new MutableGroup(module, fields))
  }

  get expression(): Ast | undefined {
    return this.module.getAst(this.fields.get('expression')?.node)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  declare fields: FixedMap<AstFields & NumericLiteralFields>
  constructor(module: Module, fields: FixedMap<AstFields & NumericLiteralFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, tokens: NodeChild<Token>[], id?: AstId | undefined) {
    const base = module.baseObject('NumericLiteral', id)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {}
}
export interface MutableNumericLiteral extends NumericLiteral, MutableAst {}
applyMixins(MutableNumericLiteral, [MutableAst])

/** The actual contents of an `ArgumentDefinition` are complex, but probably of more interest to the compiler than the
 *  GUI. We just need to represent them faithfully and create the simple cases. */
type ArgumentDefinition = NodeChild<Ast | Token>[]
type RawArgumentDefinition = NodeChild[]
type OwnedArgumentDefinition = NodeChild<Owned<MutableAst> | Token>[]

function argumentDefinitionsToRaw(
  module: MutableModule,
  defs: OwnedArgumentDefinition[],
  parent: SyncId,
): RawArgumentDefinition[] {
  return defs.map((def) =>
    def.map((part) => ({
      ...part,
      node: part.node instanceof Token ? part.node : claimChild(module, part.node, parent),
    })),
  )
}

type FunctionFields = {
  name: NodeChild<SyncId>
  argumentDefinitions: RawArgumentDefinition[]
  equals: NodeChild<SyncTokenId>
  body: NodeChild<SyncId> | undefined
}
export class Function extends Ast {
  declare fields: FixedMap<AstFields & FunctionFields>
  constructor(module: Module, fields: FixedMap<AstFields & FunctionFields>) {
    super(module, fields)
  }

  get name(): Ast {
    return this.module.getAst(this.fields.get('name').node)
  }
  get body(): Ast | undefined {
    return this.module.getAst(this.fields.get('body')?.node)
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
    name: NodeChild<Owned<MutableAst>>,
    argumentDefinitions: OwnedArgumentDefinition[],
    equals: NodeChild<Token>,
    body: NodeChild<Owned<MutableAst>> | undefined,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Function', id)
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
    name: Token | string,
    argumentDefinitions: OwnedArgumentDefinition[],
    body: Owned<MutableAst>,
  ): Owned<MutableFunction> {
    return MutableFunction.concrete(
      module,
      unspaced(PropertyAccess.Sequence([name], module)),
      argumentDefinitions,
      spaced(makeEquals()),
      autospaced(body),
    )
  }

  /** Construct a function with simple (name-only) arguments and a body block. */
  static fromStatements(
    module: MutableModule,
    name: Token | string,
    argumentNames: (Token | string)[],
    statements: Owned<MutableAst>[],
    trailingNewline?: boolean,
  ): Owned<MutableFunction> {
    const statements_: OwnedBlockLine[] = statements.map((statement) => ({
      expression: unspaced(statement),
    }))
    if (trailingNewline) {
      statements_.push({ expression: undefined })
    }
    const argumentDefinitions = argumentNames.map((name) => [unspaced(Ident.new(module, name))])
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
    this.fields.set('argumentDefinitions', argumentDefinitionsToRaw(this.module, defs, this.syncId))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  pattern: NodeChild<SyncId>
  equals: NodeChild<SyncTokenId>
  expression: NodeChild<SyncId>
}
export class Assignment extends Ast {
  declare fields: FixedMap<AstFields & AssignmentFields>
  constructor(module: Module, fields: FixedMap<AstFields & AssignmentFields>) {
    super(module, fields)
  }

  static concrete(
    module: MutableModule,
    pattern: NodeChild<Owned<MutableAst>>,
    equals: NodeChild<Token>,
    expression: NodeChild<Owned<MutableAst>>,
    id?: AstId | undefined,
  ) {
    const base = module.baseObject('Assignment', id)
    const id_ = base.get('id')
    const fields = setAll(base, {
      pattern: concreteChild(module, pattern, id_),
      equals,
      expression: concreteChild(module, expression, id_),
    })
    return asOwned(new MutableAssignment(module, fields))
  }

  static new(module: MutableModule, ident: string | Token, expression: Owned<MutableAst>) {
    return Assignment.concrete(
      module,
      unspaced(Ident.new(module, ident)),
      spaced(makeEquals()),
      spaced(expression),
    )
  }

  get pattern(): Ast {
    return this.module.getAst(this.fields.get('pattern').node)
  }
  get expression(): Ast {
    return this.module.getAst(this.fields.get('expression').node)
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

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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
  declare fields: FixedMap<AstFields & BodyBlockFields>
  constructor(module: Module, fields: FixedMap<AstFields & BodyBlockFields>) {
    super(module, fields)
  }

  static concrete(module: MutableModule, lines: OwnedBlockLine[], id?: AstId | undefined) {
    const base = module.baseObject('BodyBlock', id)
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
      yield line.newline ?? { node: new Token('\n', newTokenId(), RawAst.Token.Type.Newline) }
      if (line.expression) yield line.expression
    }
  }

  printSubtree(info: InfoMap, offset: number, parentIndent: string | undefined): string {
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
        const lineNode = this.module.getAst(line.expression.node)
        assertEqual(syncId(lineNode), line.expression.node)
        assertEqual(parentId(lineNode), this.syncId)
        code += lineNode.printSubtree(info, offset + code.length, blockIndent)
      }
    }
    const span = nodeKey(offset, code.length)
    const infos = info.nodes.get(span)
    if (infos == undefined) {
      info.nodes.set(span, [this.exprId])
    } else {
      infos.unshift(this.exprId)
    }
    return code
  }
}

export class MutableBodyBlock extends BodyBlock implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & BodyBlockFields>

  takeLines(): OwnedBlockLine[] {
    return this.fields.get('lines').map((line) => ownedLineFromRaw(line, this.module))
  }
  setLines(lines: OwnedBlockLine[]) {
    this.fields.set(
      'lines',
      lines.map((line) => lineToRaw(line, this.module, this.syncId)),
    )
  }

  /** Insert the given statement(s) starting at the specified line index. */
  insert(index: number, ...statements: Owned<MutableAst>[]) {
    const before = this.fields.get('lines').slice(0, index)
    const insertions = statements.map((statement) => ({
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement)),
    }))
    const after = this.fields.get('lines').slice(index)
    this.fields.set('lines', [...before, ...insertions, ...after])
  }

  push(statement: Owned<MutableAst>) {
    const oldLines = this.fields.get('lines')
    const newLine = {
      newline: unspaced(Token.new('\n', RawAst.Token.Type.Newline)),
      expression: unspaced(this.claimChild(statement)),
    }
    this.fields.set('lines', [...oldLines, newLine])
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {
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

type RawBlockLine = RawLine<SyncId>
type BlockLine = Line<Ast>
type OwnedBlockLine = Line<Owned<MutableAst>>

function lineFromRaw(raw: RawBlockLine, module: Module): BlockLine {
  const expression = raw.expression ? module.getAst(raw.expression.node) : undefined
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
    ? module.getAst(raw.expression.node).takeIfParented()
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

function lineToRaw(line: OwnedBlockLine, module: MutableModule, block: SyncId): RawBlockLine {
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
  declare fields: FixedMap<AstFields & IdentFields>
  constructor(module: Module, fields: FixedMap<AstFields & IdentFields>) {
    super(module, fields)
  }

  get token(): Token {
    return this.module.getToken(this.fields.get('token').node)
  }

  static concrete(module: MutableModule, token: NodeChild<Token>, id?: AstId | undefined) {
    const base = module.baseObject('Ident', id)
    const fields = setAll(base, { token })
    return asOwned(new MutableIdent(module, fields))
  }

  static new(module: MutableModule, ident: Token | string) {
    return Ident.concrete(module, unspaced(toIdent(ident)))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }
}

export class MutableIdent extends Ident implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & IdentFields>

  setToken(ident: Token | string) {
    this.fields.set('token', unspaced(toIdent(ident)))
  }

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {}
}
export interface MutableIdent extends Ident, MutableAst {}
applyMixins(MutableIdent, [MutableAst])

type WildcardFields = {
  token: NodeChild<SyncTokenId>
}
export class Wildcard extends Ast {
  declare fields: FixedMap<AstFields & WildcardFields>
  constructor(module: Module, fields: FixedMap<AstFields & WildcardFields>) {
    super(module, fields)
  }

  get token(): Token {
    return this.module.getToken(this.fields.get('token').node)
  }

  static concrete(module: MutableModule, token: NodeChild<Token>, id?: AstId | undefined) {
    const base = module.baseObject('Wildcard', id)
    const fields = setAll(base, { token })
    return asOwned(new MutableWildcard(module, fields))
  }

  static new(module: MutableModule) {
    const token = new Token('_', newTokenId(), RawAst.Token.Type.Wildcard)
    return this.concrete(module, unspaced(token))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }
}

export class MutableWildcard extends Wildcard implements MutableAst {
  declare readonly module: MutableModule
  declare readonly fields: FixedMap<AstFields & WildcardFields>

  replaceChild<T extends MutableAst>(target: SyncId, replacement: Owned<T>) {}
}
export interface MutableWildcard extends Wildcard, MutableAst {}
applyMixins(MutableWildcard, [MutableAst])

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
export function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v"'`]/g, (match) => mapping[match]!)
}

function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  info: InfoMap | undefined,
): { whitespace: string | undefined; node: Ast } {
  const nodesExpected = new Map(
    Array.from(info?.nodes.entries() ?? [], ([span, ids]) => [span, [...ids]]),
  )
  const tokens = info?.tokens ?? new Map()
  const tokensOut = info?.tokensOut ?? new Map()
  return abstractTree(module, tree, code, nodesExpected, tokens, tokensOut)
}

function abstractTree(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  nodesExpected: NodeSpanMap,
  tokens: TokenSpanMap,
  tokensOut: TokenSpanMap,
): { whitespace: string | undefined; node: Owned<MutableAst> } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesExpected, tokens, tokensOut)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokens, tokensOut)
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild<Owned<MutableAst> | Token>[] = []
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
  const codeStart = whitespaceEnd
  const codeEnd = codeStart + tree.childrenLengthInCodeParsed
  // All node types use this value in the same way to obtain the ID type,
  // but each node does so separately because we must pop the tree's span from the ID map
  // *after* processing children.
  const spanKey = nodeKey(codeStart, codeEnd - codeStart)
  let node: Owned<MutableAst>
  switch (tree.type) {
    case RawAst.Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = recurseToken(line.newline)
        let expression = line.expression ? recurseTree(line.expression) : undefined
        return { newline, expression }
      })
      const id = nodesExpected.get(spanKey)?.pop()
      node = BodyBlock.concrete(module, lines, id)
      break
    }
    case RawAst.Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const argumentDefinitions = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body = tree.body !== undefined ? recurseTree(tree.body) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      node = Function.concrete(module, name, argumentDefinitions, equals, body, id)
      break
    }
    case RawAst.Tree.Type.Ident: {
      const token = recurseToken(tree.token)
      const id = nodesExpected.get(spanKey)?.pop()
      node = Ident.concrete(module, token, id)
      break
    }
    case RawAst.Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      const id = nodesExpected.get(spanKey)?.pop()
      node = Assignment.concrete(module, pattern, equals, value, id)
      break
    }
    case RawAst.Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      const id = nodesExpected.get(spanKey)?.pop()
      node = App.concrete(module, func, undefined, undefined, arg, id)
      break
    }
    case RawAst.Tree.Type.NamedApp: {
      const func = recurseTree(tree.func)
      const open = tree.open ? recurseToken(tree.open) : undefined
      const name = recurseToken(tree.name)
      const equals = recurseToken(tree.equals)
      const arg = recurseTree(tree.arg)
      const close = tree.close ? recurseToken(tree.close) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      const parens = open && close ? { open, close } : undefined
      const nameSpecification = { name, equals }
      node = App.concrete(module, func, parens, nameSpecification, arg, id)
      break
    }
    case RawAst.Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      if (arg && opr.node.code() === '-') {
        node = NegationOprApp.concrete(module, opr, arg, id)
      } else {
        node = UnaryOprApp.concrete(module, opr, arg, id)
      }
      break
    }
    case RawAst.Tree.Type.OprApp: {
      const lhs = tree.lhs ? recurseTree(tree.lhs) : undefined
      const opr = tree.opr.ok
        ? [recurseToken(tree.opr.value)]
        : Array.from(tree.opr.error.payload.operators, recurseToken)
      const rhs = tree.rhs ? recurseTree(tree.rhs) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.length === 1 && opr[0]?.node.code() === '.' && rhs?.node instanceof MutableIdent) {
        // Propagate type.
        const rhs_ = { ...rhs, node: rhs.node }
        node = PropertyAccess.concrete(module, lhs, opr[0], rhs_, id)
      } else {
        node = OprApp.concrete(module, lhs, opr, rhs, id)
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
      const id = nodesExpected.get(spanKey)?.pop()
      node = NumericLiteral.concrete(module, tokens, id)
      break
    }
    case RawAst.Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      const id = nodesExpected.get(spanKey)?.pop()
      node = Wildcard.concrete(module, token, id)
      break
    }
    // These expression types are (or will be) used for backend analysis.
    // The frontend can ignore them, avoiding some problems with expressions sharing spans
    // (which makes it impossible to give them unique IDs in the current IdMap format).
    case RawAst.Tree.Type.OprSectionBoundary:
    case RawAst.Tree.Type.TemplateFunction: {
      node = recurseTree(tree.ast).node
      break
    }
    case RawAst.Tree.Type.Invalid: {
      const expression = recurseTree(tree.ast)
      const id = nodesExpected.get(spanKey)?.pop()
      node = Invalid.concrete(module, expression, id)
      break
    }
    case RawAst.Tree.Type.Group: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const expression = tree.body ? recurseTree(tree.body) : undefined
      const close = tree.close ? recurseToken(tree.close) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      node = Group.concrete(module, open, expression, close, id)
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
      const id = nodesExpected.get(spanKey)?.pop()
      node = TextLiteral.concrete(module, open, newline, elements, close, id)
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
      const id = nodesExpected.get(spanKey)?.pop()
      node = Documented.concrete(module, open, elements, newlines, expression, id)
      break
    }
    case RawAst.Tree.Type.Import: {
      const recurseBody = (tree: RawAst.Tree) => {
        const body = recurseTree(tree)
        if (body instanceof Invalid && body.code() === '') return undefined
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
      const id = nodesExpected.get(spanKey)?.pop()
      node = Import.concrete(module, polyglot, from, import_, all, as, hiding, id)
      break
    }
    default: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = Generic.concrete(module, visitChildren(tree), id)
    }
  }
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  return { node, whitespace }
}

function abstractToken(
  token: RawAst.Token,
  code: string,
  tokens: TokenSpanMap,
  tokensOut: TokenSpanMap,
): { whitespace: string; node: Token } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const exprId = tokens.get(key) ?? newTokenId()
  const node = new Token(tokenCode, exprId, token.type)
  tokensOut.set(key, exprId)
  return { whitespace, node }
}

declare const nodeKeyBrand: unique symbol
type NodeKey = string & { [nodeKeyBrand]: never }
declare const tokenKeyBrand: unique symbol
type TokenKey = string & { [tokenKeyBrand]: never }
function nodeKey(start: number, length: number): NodeKey {
  return `${start}:${length}` as NodeKey
}
function tokenKey(start: number, length: number): TokenKey {
  return `${start}:${length}` as TokenKey
}
export function keyToRange(key: NodeKey | TokenKey): { start: number; end: number } {
  const parts = key.split(':')
  const start = parseInt(parts[0]!, 10)
  const length = parseInt(parts[1]!, 10)
  return { start, end: start + length }
}

interface SerializedInfoMap {
  nodes: Record<NodeKey, AstId[]>
  tokens: Record<TokenKey, TokenId>
}

interface SerializedPrintedSource {
  info: SerializedInfoMap
  code: string
}

type NodeSpanMap = Map<NodeKey, AstId[]>
type TokenSpanMap = Map<TokenKey, TokenId>

export interface InfoMap {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
  tokensOut: TokenSpanMap
}

interface PrintedSource {
  info: InfoMap
  code: string
}

/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
    tokensOut: new Map(),
  }
  const code = ast.printSubtree(info, 0, undefined)
  return { info, code }
}

export type TokenTree = (TokenTree | string)[]
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (isTokenId(child.node)) {
      return module.getToken(child.node).code()
    } else {
      const node = module.tryGetAst(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.exprId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (isTokenId(child.node)) {
        return module.getToken(child.node).code()
      } else {
        const node = module.tryGetAst(child.node)
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

export function parseTransitional(code: string, idMap: IdMap): Ast {
  const rawAst = RawAstExtended.parse(code, idMap.clone())
  const nodes = new Map<NodeKey, AstId[]>()
  const tokens = new Map<TokenKey, TokenId>()
  const astExtended = new Map<AstId, RawAstExtended>()
  const spans = new Map<AstId, SourceRange>()
  rawAst.visitRecursive((nodeOrToken: RawAstExtended<RawAst.Tree | RawAst.Token>) => {
    const start = nodeOrToken.span()[0]
    const length = nodeOrToken.span()[1] - nodeOrToken.span()[0]
    if (nodeOrToken.isToken()) {
      const token: RawAstExtended<RawAst.Token> = nodeOrToken
      tokens.set(tokenKey(start, length), token.astId as TokenId)
    } else if (nodeOrToken.isTree()) {
      const node: RawAstExtended<RawAst.Tree> = nodeOrToken
      if (
        node.isTree(RawAst.Tree.Type.OprSectionBoundary) ||
        node.isTree(RawAst.Tree.Type.TemplateFunction)
      )
        return true
      let id = node.astId as AstId
      const preexisting = astExtended.get(id)
      if (preexisting) {
        if (!preexisting.isTree(RawAst.Tree.Type.Invalid)) {
          console.warn(`Unexpected duplicate UUID in tree`, id)
        }
        id = newAstId()
      }
      astExtended.set(id, node)
      spans.set(id, node.span())
      const key = nodeKey(start, length)
      const ids = nodes.get(key)
      if (ids !== undefined) {
        ids.push(id)
      } else {
        nodes.set(key, [id])
      }
    }
    return true
  })
  const tokensOut = new Map()
  const newRoot = parseBlock({ info: { nodes, tokens, tokensOut }, code })
  newRoot.module.spans = spans
  idMap.clear()
  // TODO (optimization): Use ID-match info collected while abstracting.
  /*
  for (const [id, ast] of newRoot.module.raw.nodes.entries()) {
    idMap.insertKnownId( ... )
  }
  for (const [key, id] of tokensOut) {
    idMap.insertKnownId( ... )
  }
   */
  const printed = print(newRoot)
  for (const [key, ids] of printed.info.nodes) {
    const range = keyToRange(key)
    idMap.insertKnownId([range.start, range.end], ids[0]!)
  }
  for (const [key, id] of printed.info.tokens) {
    const range = keyToRange(key)
    idMap.insertKnownId([range.start, range.end], id)
  }
  return newRoot
}

/** Parse the input as a block. */
export function parseBlock(source: PrintedSource | string, inModule?: MutableModule) {
  const code = typeof source === 'object' ? source.code : source
  const ids = typeof source === 'object' ? source.info : undefined
  const tree = parseEnso(code)
  const module = inModule ?? MutableModule.Transient()
  const ast = abstract(module, tree, code, ids).node
  // The root of the tree produced by the parser is always a `BodyBlock`.
  const block = ast as MutableBodyBlock
  return asOwned(block)
}

/** Parse the input. If it contains a single expression at the top level, return it; otherwise, return a block. */
export function parse(source: PrintedSource | string, module?: MutableModule): Owned<MutableAst> {
  const module_ = module ?? MutableModule.Transient()
  const ast = parseBlock(source, module_)
  const [expr] = ast.statements()
  if (!expr) return ast
  const parent = parentId(expr)
  if (parent) module_.delete(parent)
  expr.fields.set('parent', undefined)
  return asOwned(expr)
}

export function deserialize(serialized: string): Ast {
  return Ast.deserialize(serialized)
}

declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [TokenKey]: Token
  }
}
