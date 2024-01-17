import * as RawAst from '@/generated/ast'
import { assert, assertEqual, bail } from '@/util/assert'
import { parseEnso } from '@/util/ast'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import type { Opt } from '@/util/data/opt'
import { Err, Ok, type Result } from '@/util/data/result'
import type { LazyObject } from '@/util/parserSupport'
import { unsafeEntries } from '@/util/record'
import { createIterator, mapIterator } from 'lib0/iterator'
import * as map from 'lib0/map'
import * as random from 'lib0/random'
import { reactive } from 'vue'
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
export type Owned<T> = T & { [brandOwned]: never }
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

  iterAllNodeIds(): IterableIterator<AstId> {
    const parentIter = this.base?.iterAllNodeIds()
    let parentDone = parentIter == null
    const nodeIter = this.nodes.entries()
    const editedVisited = new Set()
    return createIterator(() => {
      for (;;) {
        if (!parentDone) {
          const result = parentIter?.next()
          if (result?.done) parentDone = true
          else if (result?.value) {
            const id = result.value
            const edited = this.nodes.get(id)
            if (edited !== undefined) editedVisited.add(id)
            if (edited === null) continue
            return result
          }
        } else {
          const next = nodeIter.next()
          if (next.done === true) return next
          const [id, ast] = next.value
          if (ast !== null && !editedVisited.has(id)) {
            return {
              done: false,
              value: id,
            }
          }
        }
      }
    })
  }

  iterAllNodes(): IterableIterator<Ast> {
    return mapIterator(this.iterAllNodeIds(), (id) => {
      const node = this.get(id)
      assert(node != null)
      return node
    })
  }
}

export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn.exprId, rootIn.module)
  const module = Module.Transient()
  const tree = parseEnso(printed.code)
  return abstract(module, tree, printed.code, printed.info).node
}
*/

export type NodeChild<T = AstId | Token> = { whitespace?: string | undefined; node: T }

declare const brandAstId: unique symbol
declare const brandTokenId: unique symbol
export type AstId = ExprId & { [brandAstId]: never }
export type TokenId = ExprId & { [brandTokenId]: never }

function newAstId(): AstId {
  return random.uuidv4() as AstId
}
function newTokenId(): TokenId {
  return random.uuidv4() as TokenId
}
// Cast an ID which may be a tree or a token to a tree ID.
export function asNodeId(expr: ExprId): AstId {
  return expr as AstId
}

export class Token {
  leadingSpace: string | undefined
  readonly exprId: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined
  constructor(code: string, id: TokenId, type: RawAst.Token.Type | undefined) {
    this.exprId = id
    this.code_ = code
    this.tokenType_ = type
  }

  static new(code: string) {
    return new Token(code, newTokenId(), undefined)
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

function spaced<T>(node: T): NodeChild<T> {
  return { whitespace: ' ', node }
}

function unspaced<T>(node: T): NodeChild<T> {
  return { whitespace: '', node }
}

function autospaced<T>(node: T): NodeChild<T> {
  return { node }
}

function spacedIf<T>(node: T, isSpaced: boolean): NodeChild<T> {
  return { whitespace: isSpaced ? ' ' : '', node }
}

/*
function makeChild<T extends Ast>(module: Module, child: Owned<T>, parent: AstId): AstId {
  const oldParent = child.parentId
  assert(
    !oldParent || !module.has(oldParent),
    'Owned object is not owned. Was it obtained from a different module?',
  )
  const spliced = module.splice(child)
  spliced.parent = parent
  return spliced.exprId
}

function setParent(module: Module, parent: AstId, ...children: (NodeChild | null)[]) {
  for (const child of children) {
    if (child && !(child.node instanceof Token)) module.get(child.node)!.parent = parent
  }
}
 */

////////////////////////////////////////////////////////

export interface IModule {
  baseObject(type: string, id?: AstId): FixedMap<AstFields>
  getAst(id: AstId): Ast
  getAst(id: AstId | null): Ast | null
  getAst(id: AstId | null): Ast | null
  getExtended(id: AstId): RawAstExtended | undefined
  edit(): Module
  iterAllNodeIds(): IterableIterator<AstId>
  iterAllNodes(): IterableIterator<Ast>
  has(id: AstId): boolean
}
class Module implements IModule {
  private readonly nodes: Map<AstId, Map<string, unknown>>
  spans: Map<AstId, SourceRange> | null

  constructor(nodes: Map<AstId, Map<string, unknown>>, spans: Map<AstId, SourceRange> | null) {
    this.nodes = nodes
    this.spans = spans
  }

  baseObject(type: string, id?: AstId): FixedMap<AstFields> {
    const map = new Map<string, unknown>() as FixedMap<{}>
    return setAll(map, {
      id: id ?? newAstId(),
      type: type,
      parent: null,
    })
  }

  get(id: AstId): Ast
  get(id: AstId | null): Ast | null
  get(id: AstId | null): Ast | null {
    const nodeData = this.nodes.get(id)
    assert(nodeData !== undefined)
    const fields = nodeData as FixedMap<any>
    const type = fields.get('type')
    switch (type) {
      case 'App':
        return new App(this, fields)
      case 'UnaryOprApp':
        return new UnaryOprApp(this, fields)
      case 'OprApp':
        return new OprApp(this, fields)
      case 'Generic':
        return new Generic(this, fields)
      case 'Import':
        return new Import(this, fields)
      case 'TextLiteral':
        return new TextLiteral(this, fields)
      case 'Documented':
        return new Documented(this, fields)
      case 'Invalid':
        return new Invalid(this, fields)
      case 'Group':
        return new Group(this, fields)
      case 'NumericLiteral':
        return new NumericLiteral(this, fields)
      case 'Function':
        return new Function(this, fields)
      case 'Assignment':
        return new Assignment(this, fields)
      case 'BodyBlock':
        return new BodyBlock(this, fields)
      case 'Ident':
        return new Ident(this, fields)
      case 'Wildcard':
        return new Wildcard(this, fields)
    }
    bail(`Invalid type: ${type}`)
  }

  /** Modify the parent of `target` to refer to a new object instead of `target`. Return `target`, which now has no parent. */
  replaceRef<T extends Ast>(target: AstId, replacement: Owned<T>): Owned<Ast> | undefined {
    const old = this.get(target)
    const parentId = old.parentId
    if (!parentId) return
    const parent = this.get(parentId)
    parent.replaceChild(target, replacement)
    old.fields.set('parent', null)
    return asOwned(old)
  }

  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends Ast>(target: AstId, replacement: Owned<T>): Owned<Ast> | undefined {
    const old = this.get(target)
    const replacement_ = this.splice(replacement, target)
    replacement_.fields.set('parent', old?.parentId)
    if (replacement.module === this && replacement.exprId !== target)
      this.delete(replacement.exprId)
    if (old) {
      const old_ = this.splice(old, newAstId())
      old_.fields.set('parent', null)
      return asOwned(old_)
    } else {
      return undefined
    }
  }

  /** Replace the parent of `target`'s reference to it with a reference to a new placeholder object. Return `target`. */
  take(target: AstId): { node: Owned<Ast>; placeholder: Wildcard } | undefined {
    const placeholder = Wildcard.new(this)
    const node = this.replaceRef(target, placeholder)
    if (!node) return
    return { node, placeholder }
  }

  /** Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   **/
  takeValue(target: AstId): Owned<Ast> | undefined {
    return this.replaceValue(target, Wildcard.new(this))
  }

  takeAndReplaceRef<T extends Ast>(target: AstId, wrap: (x: Owned<Ast>) => Owned<T>): T {
    const taken = this.take(target)
    assert(!!taken)
    const replacement = wrap(taken.node)
    this.replaceRef(taken.placeholder.exprId, replacement)
    return replacement
  }

  takeAndReplaceValue<T extends Ast>(target: AstId, wrap: (x: Owned<Ast>) => Owned<T>): T {
    const taken = this.takeValue(target)
    assert(!!taken)
    const replacement = wrap(taken)
    this.replaceValue(target, replacement)
    return this.get(target)! as T
  }

  /** Copy the given node and all its descendants into this module. */
  splice(ast: Ast, id?: AstId): Ast
  splice(ast: Ast | null, id?: AstId): Ast | null
  splice(ast: Ast | undefined, id?: AstId): Ast | undefined
  splice(ast: Ast | null | undefined, id?: AstId): Ast | null | undefined {
    if (!ast) return ast
    const id_ = id ?? ast.exprId
    if (ast.module === this && this.get(id_) === ast) return ast
    const ast_ = ast.cloneWithId(this, id_)
    for (const child of ast_.concreteChildren()) {
      if (!(child.node instanceof Token)) {
        const childInForeignModule = ast.module.get(child.node)
        assert(childInForeignModule !== null)
        const child_ = this.splice(childInForeignModule)
        child_.fields.set('parent', id_)
      }
    }
    this.nodes.set(id_, ast_.fields as any)
    return ast_
  }

  spliceShallow(ast: Ast): Ast {
    const cloned = ast.cloneWithId(this, ast.exprId)
    this.nodes.set(ast.exprId, cloned.fields as any)
    return cloned
  }

  delete(id: AstId) {
    this.nodes.delete(id)
  }

  has(id: AstId) {
    return this.nodes.has(id)
  }
}

type FixedMap<Fields> = {
  get<Key extends string & keyof Fields>(key: Key): Fields[Key]
  set<Key extends string & keyof Fields>(key: Key, value: Fields[Key]): void
  entries(): IterableIterator<readonly [string, unknown]>
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

type AstFields = {
  id: AstId
  type: string
  parent: AstId | null
}
export abstract class Ast {
  readonly module: Module
  readonly fields: FixedMap<AstFields>

  get exprId(): AstId {
    return this.fields.get('id')
  }

  get parentId(): AstId | null {
    return this.fields.get('parent')
  }

  /**
   * Return whether `this` and `other` are the same object.
   * This is true if and only if they are in the same module and have the same `exprId`.
   * If true, changes to either will be immediately visible in the other.
   *
   * NOTE: This method should be used in place of `===`.
   */
  is<T extends Ast>(other: T): boolean {
    return this.fields === other.fields
  }

  /** Returns child subtrees, including information about the whitespace between them. */
  abstract concreteChildren(): IterableIterator<NodeChild>
  abstract replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>): void

  protected constructor(module: Module, fields: FixedMap<AstFields>) {
    this.module = module
    this.fields = fields
  }

  protected claimChild<T extends Ast>(child: Owned<T>): AstId
  protected claimChild<T extends Ast>(child: Owned<T> | null): AstId | null
  protected claimChild<T extends Ast>(child: Owned<T> | null): AstId | null {
    return child ? claimChild(this.module, child, this.exprId) : null
  }

  ////////////////////

  serialize(): string {
    return JSON.stringify(print(this.exprId, this.module))
  }

  static deserialize(serialized: string): Ast {
    const parsed: SerializedPrintedSource = JSON.parse(serialized)
    const nodes = new Map(unsafeEntries(parsed.info.nodes))
    const tokens = new Map(unsafeEntries(parsed.info.tokens))
    const tokensOut = new Map()
    const module = Module.Transient()
    const tree = parseEnso(parsed.code)
    return abstract(module, tree, parsed.code, { nodes, tokens, tokensOut }).node
  }

  /** Return this node's span, if it belongs to a module with an associated span map. */
  get span(): SourceRange | undefined {
    return this.module.spans?.get(this.exprId)
  }

  /** Returns child subtrees, without information about the whitespace between them. */
  *children(): IterableIterator<Ast | Token> {
    for (const child of this.concreteChildren()) {
      if (child.node instanceof Token) {
        yield child.node
      } else {
        const node = this.module.get(child.node)
        if (node) yield node
      }
    }
  }

  innerExpression(): Ast {
    // TODO: Override this in `Documented`, `Annotated`, `AnnotatedBuiltin`
    return this
  }

  code(module?: Module): string {
    return print(this.exprId, module ?? this.module).code
  }

  typeName(): string | undefined {
    if (this.treeType === undefined) return undefined
    return RawAst.Tree.typeNames[this.treeType]
  }

  /** Parse the input as a block. */
  static parseBlock(source: PrintedSource | string, inModule?: Module) {
    const code = typeof source === 'object' ? source.code : source
    const ids = typeof source === 'object' ? source.info : undefined
    const tree = parseEnso(code)
    const module = inModule ?? Module.Transient()
    const ast = abstract(module, tree, code, ids).node
    // The root of the tree produced by the parser is always a `BodyBlock`.
    const block = ast as BodyBlock
    return asOwned(block)
  }

  /** Parse the input. If it contains a single expression at the top level, return it; otherwise, return a block. */
  static parse(source: PrintedSource | string, module?: Module): Owned<Ast> {
    const module_ = module ?? Module.Transient()
    const ast = Ast.parseBlock(source, module_)
    const [expr] = ast.statements()
    if (!expr) return ast
    if (expr.parentId) module_.nodes.delete(expr.parentId)
    expr.fields.set('parent', null)
    return asOwned(expr)
  }

  visitRecursive(visit: (node: Ast | Token) => void) {
    visit(this)
    for (const child of this.concreteChildren()) {
      if (child.node instanceof Token) {
        visit(child.node)
      } else {
        this.module.get(child.node)?.visitRecursive(visit)
      }
    }
  }

  printSubtree(
    info: InfoMap,
    offset: number,
    parentIndent: string | null,
    moduleOverride?: Module | undefined,
  ): string {
    const module_ = moduleOverride ?? this.module
    let code = ''
    for (const child of this.concreteChildren()) {
      if (!(child.node instanceof Token) && module_.get(child.node) === null) continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        code += ' '
      }
      if (child.node instanceof Token) {
        const tokenStart = offset + code.length
        const tokenCode = child.node.code()
        const span = tokenKey(tokenStart, tokenCode.length)
        info.tokens.set(span, child.node.exprId)
        code += tokenCode
      } else {
        const childNode = module_.get(child.node)
        assert(childNode != null)
        code += childNode.printSubtree(info, offset + code.length, parentIndent, moduleOverride)
        // Extra structural validation.
        assertEqual(childNode.exprId, child.node)
        if (childNode.parentId !== this.exprId) {
          console.error(`Inconsistent parent pointer (expected ${this.exprId})`, childNode)
        }
        assertEqual(childNode.parentId, this.exprId)
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = map.setIfUndefined(info.nodes, span, (): AstId[] => [])
    infos.unshift(this.exprId)
    return code
  }

  /** Return a copy of this node, with the specified `module` and `exprId` properties.
   *
   *  The node's owned data is deep-copied, although note that child subtrees are stored as IDs,
   *  so a full deep copy requires recursively cloning child nodes.
   */
  cloneWithId(module: Module, exprId: AstId): this {
    const cloned = clone(this)
    // No one else has a reference to this object, so we can ignore `readonly` and mutate it.
    Object.assign(cloned, { module, exprId })
    return cloned
  }
}

function claimChild(module: Module, child: Owned<Ast>, parent: AstId): AstId {
  if (child.module !== module) {
    throw new Error('TODO')
  }
  child.fields.set('parent', parent)
  return child.exprId
}

function concreteChild(
  module: Module,
  child: NodeChild<Owned<Ast>>,
  parent: AstId,
): NodeChild<AstId> {
  return { ...child, node: claimChild(module, child.node, parent) }
}

function toIdent(ident: string | Token): Token
function toIdent(ident: string | Token | null): Token | null
function toIdent(ident: string | Token | null): Token | null {
  return ident
    ? ident instanceof Token
      ? ident
      : new Token(ident, newTokenId(), RawAst.Token.Type.Ident)
    : null
}
function makeEquals(): Token {
  return new Token('=', newTokenId(), RawAst.Token.Type.Operator)
}

function nameSpecification(
  name: string | Token | null,
): { name: NodeChild<Token>; equals: NodeChild<Token> } | null {
  if (name === null) return null
  return { name: autospaced(toIdent(name)), equals: unspaced(makeEquals()) }
}

type AppFields = {
  function: NodeChild<AstId>
  parens: { open: NodeChild<Token>; close: NodeChild<Token> } | null
  nameSpecification: { name: NodeChild<Token>; equals: NodeChild<Token> } | null
  argument: NodeChild<AstId>
}
export class App extends Ast {
  declare fields: FixedMap<AstFields & AppFields>
  constructor(module: Module, fields: FixedMap<AstFields & AppFields>) {
    super(module, fields)
  }

  static new(
    module: Module,
    func: Owned<Ast>,
    argumentName: string | Token | null,
    argument: Owned<Ast>,
  ) {
    const base = module.baseObject('App')
    const id = base.get('id')
    const fields = setAll(base, {
      function: unspaced(claimChild(module, func, id)),
      parens: null,
      nameSpecification: nameSpecification(argumentName),
      argument: autospaced(claimChild(module, argument, id)),
    })
    return asOwned(new App(module, fields))
  }

  get function(): Ast {
    return this.module.get(this.fields.get('function').node)
  }
  setFunction<T extends Ast>(func: Owned<T>) {
    this.fields.set('function', unspaced(this.claimChild(func)))
  }
  get argumentName(): Token | null {
    return this.fields.get('nameSpecification')?.name.node ?? null
  }
  setArgumentName(name: string | Token | null) {
    this.fields.set('nameSpecification', nameSpecification(name))
  }
  get argument(): Ast {
    return this.module.get(this.fields.get('argument').node)
  }
  setArgument<T extends Ast>(argument: Owned<T>) {
    this.fields.set('argument', { node: this.claimChild(argument) })
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

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('function').node === target) {
      this.setFunction(replacement)
    } else if (this.fields.get('argument').node === target) {
      this.setArgument(replacement)
    }
  }
}

function positionalApp(
  module: Module,
  id: AstId | undefined,
  func: NodeChild<Owned<Ast>>,
  argument: NodeChild<Owned<Ast>>,
) {
  const base = module.baseObject('App', id)
  const id_ = base.get('id')
  const fields = setAll(base, {
    function: concreteChild(module, func, id_),
    parens: null,
    nameSpecification: null,
    argument: concreteChild(module, argument, id_),
  })
  return asOwned(new App(module, fields))
}

function namedApp(
  module: Module,
  id: AstId | undefined,
  func: NodeChild<Owned<Ast>>,
  open: NodeChild<Token> | null,
  name: NodeChild<Token>,
  equals: NodeChild<Token>,
  argument: NodeChild<Owned<Ast>>,
  close: NodeChild<Token> | null,
) {
  const base = module.baseObject('App', id)
  const id_ = base.get('id')
  const fields = setAll(base, {
    function: concreteChild(module, func, id_),
    parens: open && close ? { open, close } : null,
    nameSpecification: { name, equals },
    argument: concreteChild(module, argument, id_),
  })
  return asOwned(new App(module, fields))
}

type UnaryOprAppFields = {
  operator: NodeChild<Token>
  argument: NodeChild<AstId> | null
}
export class UnaryOprApp extends Ast {
  declare fields: FixedMap<AstFields & UnaryOprAppFields>
  constructor(module: Module, fields: FixedMap<AstFields & UnaryOprAppFields>) {
    super(module, fields)
  }

  static new(module: Module, operator: Token, argument: Owned<Ast> | null) {
    const base = module.baseObject('UnaryOprApp')
    const id = base.get('id')
    const fields = setAll(base, {
      operator: unspaced(operator),
      argument: argument ? autospaced(claimChild(module, argument, id)) : null,
    })
    return asOwned(new UnaryOprApp(module, fields))
  }

  get operator(): Token {
    return this.fields.get('operator').node
  }
  setOperator(value: Token) {
    this.fields.set('operator', unspaced(value))
  }
  get argument(): Ast | null {
    return this.module.get(this.fields.get('argument')?.node ?? null)
  }
  setArgument<T extends Ast>(argument: Owned<T>) {
    this.fields.set('argument', autospaced(this.claimChild(argument)))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { operator, argument } = getAll(this.fields)
    yield operator
    if (argument) yield argument
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('argument')?.node === target) {
      this.setArgument(replacement)
    }
  }
}

export class NegationOprApp extends UnaryOprApp {
  static new(module: Module, operator: Token, argument: Owned<Ast> | null): Owned<NegationOprApp> {
    const base = module.baseObject('NegationOprApp')
    const id = base.get('id')
    const fields = setAll(base, {
      operator: unspaced(operator),
      argument: argument ? autospaced(claimChild(module, argument, id)) : null,
    })
    return asOwned(new NegationOprApp(module, fields))
  }
}

type OprAppFields = {
  lhs: NodeChild<AstId> | null
  operator: NodeChild[]
  rhs: NodeChild<AstId> | null
}
export class OprApp extends Ast {
  declare fields: FixedMap<AstFields & OprAppFields>
  constructor(module: Module, fields: FixedMap<AstFields & OprAppFields>) {
    super(module, fields)
  }

  static new(module: Module, lhs: Owned<Ast> | null, operator: Token, rhs: Owned<Ast> | null) {
    const base = module.baseObject('OprApp')
    const id = base.get('id')
    const fields = setAll(base, {
      lhs: lhs ? autospaced(claimChild(module, lhs, id)) : null,
      operator: [unspaced(operator)],
      rhs: rhs ? autospaced(claimChild(module, rhs, id)) : null,
    })
    return asOwned(new OprApp(module, fields))
  }

  get lhs(): Ast | null {
    return this.module.get(this.fields.get('lhs')?.node ?? null)
  }
  setLhs<T extends Ast>(value: Owned<T>) {
    this.fields.set('lhs', autospaced(this.claimChild(value)))
  }
  get operator(): Result<Token, NodeChild[]> {
    const operators = this.fields.get('operator')
    const [opr] = operators
    return opr?.node instanceof Token ? Ok(opr.node) : Err(operators)
  }
  setOperator(value: Token) {
    this.fields.set('operator', [unspaced(value)])
  }
  get rhs(): Ast | null {
    return this.module.get(this.fields.get('lhs')?.node ?? null)
  }
  setRhs<T extends Ast>(value: Owned<T>) {
    this.fields.set('rhs', autospaced(this.claimChild(value)))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { lhs, operator, rhs } = getAll(this.fields)
    if (lhs) yield lhs
    yield* operator
    if (rhs) yield rhs
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    if (this.fields.get('lhs')?.node === target) {
      this.setLhs(replacement)
    } else if (this.fields.get('rhs')?.node === target) {
      this.setRhs(replacement)
    }
  }
}

export class PropertyAccess extends OprApp {
  static new(module: Module, lhs: Owned<Ast>, operator: Token | undefined, rhs: Token | string) {
    const base = module.baseObject('PropertyAccess')
    const id = base.get('id')
    const fields = setAll(base, {
      lhs: autospaced(claimChild(module, lhs, id)),
      operator: [unspaced(operator ?? new Token('.', newTokenId(), RawAst.Token.Type.Operator))],
      rhs: autospaced(claimChild(module, Ident.new(module, toIdent(rhs)), id)),
    })
    return asOwned(new PropertyAccess(module, fields))
  }

  static Sequence(
    segments: [(Token | string), ...(Token | string)[]],
    module: Module,
  ): Owned<PropertyAccess> | Owned<Ident>
  static Sequence(
    segments: (Token | string)[],
    module: Module,
  ): Owned<PropertyAccess> | Owned<Ident> | undefined
  static Sequence(
    segments: (Token | string)[],
    module: Module,
  ): Owned<PropertyAccess> | Owned<Ident> | undefined {
    let path
    for (const s of segments) {
      const t = toIdent(s)
      path = path ? PropertyAccess.new(module, path, undefined, t) : Ident.new(module, t)
    }
    return path
  }
}

type GenericFields = {
  children: NodeChild[]
}
export class Generic extends Ast {
  declare fields: FixedMap<AstFields & GenericFields>
  constructor(module: Module, fields: FixedMap<AstFields & GenericFields>) {
    super(module, fields)
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.fields.get('children')[Symbol.iterator]()
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const replacement_ = autospaced(this.claimChild(replacement))
    this.fields.set(
      'children',
      this.fields.get('children').map((child) => (child.node === target ? replacement_ : child)),
    )
  }
}

type MultiSegmentAppSegment = { header: NodeChild<Token>; body: NodeChild<AstId> | null }
function multiSegmentAppSegment(header: string, body: AstId): MultiSegmentAppSegment
function multiSegmentAppSegment(header: string, body: AstId | null): MultiSegmentAppSegment | null
function multiSegmentAppSegment(header: string, body: AstId | null): MultiSegmentAppSegment | null {
  return body
    ? {
        header: { node: new Token(header, newTokenId(), RawAst.Token.Type.Ident) },
        body: spaced(body),
      }
    : null
}

type ImportFields = {
  polyglot: MultiSegmentAppSegment | null
  from: MultiSegmentAppSegment | null
  import: MultiSegmentAppSegment
  all: NodeChild<Token> | null
  as: MultiSegmentAppSegment | null
  hiding: MultiSegmentAppSegment | null
}
export class Import extends Ast {
  declare fields: FixedMap<AstFields & ImportFields>
  constructor(module: Module, fields: FixedMap<AstFields & ImportFields>) {
    super(module, fields)
  }

  get polyglot(): Ast | null {
    return this.module.get(this.fields.get('polyglot')?.body?.node ?? null)
  }
  get from(): Ast | null {
    return this.module.get(this.fields.get('from')?.body?.node ?? null)
  }
  get import_(): Ast | null {
    return this.module.get(this.fields.get('import').body?.node ?? null)
  }
  get all(): Token | null {
    return this.fields.get('all')?.node ?? null
  }
  get as(): Ast | null {
    return this.module.get(this.fields.get('as')?.body?.node ?? null)
  }
  get hiding(): Ast | null {
    return this.module.get(this.fields.get('hiding')?.body?.node ?? null)
  }

  setPolyglot<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('polyglot', multiSegmentAppSegment('polyglot', this.claimChild(value)))
  }
  setFrom<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('from', multiSegmentAppSegment('from', this.claimChild(value)))
  }
  setImport<T extends Ast>(value: Owned<T>) {
    this.fields.set('import', multiSegmentAppSegment('import', this.claimChild(value)))
  }
  setAll(value: Token | null) {
    this.fields.set('all', value ? spaced(value) : null)
  }
  setAs<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('as', multiSegmentAppSegment('as', this.claimChild(value)))
  }
  setHiding<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('hiding', multiSegmentAppSegment('hiding', this.claimChild(value)))
  }

  static Qualified(path: string[], module: Module): Owned<Import> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const base = module.baseObject('Import')
    const id = base.get('id')
    const fields = setAll(base, {
      polyglot: null,
      from: null,
      import: multiSegmentAppSegment('import', claimChild(module, path_, id)),
      all: null,
      as: null,
      hiding: null,
    })
    return asOwned(new Import(module, fields))
  }

  static Unqualified(
    path: (Token | string)[],
    name: Token | string,
    module: Module,
  ): Owned<Import> | undefined {
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const name_ = Ident.new(module, name)
    const base = module.baseObject('Import')
    const id = base.get('id')
    const fields = setAll(base, {
      polyglot: null,
      from: multiSegmentAppSegment('from', claimChild(module, path_, id)),
      import: multiSegmentAppSegment('import', claimChild(module, name_, id)),
      all: null,
      as: null,
      hiding: null,
    })
    return asOwned(new Import(module, fields))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const segment = (segment: MultiSegmentAppSegment | null) => {
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

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
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

type TextLiteralFields = {
  open: NodeChild<Token> | null
  newline: NodeChild<Token> | null
  elements: NodeChild[]
  close: NodeChild<Token> | null
}
export class TextLiteral extends Ast {
  declare fields: FixedMap<AstFields & TextLiteralFields>
  constructor(module: Module, fields: FixedMap<AstFields & TextLiteralFields>) {
    super(module, fields)
  }

  static new(rawText: string, module: Module) {
    const open = unspaced(Token.new("'"))
    const elements = [unspaced(Token.new(escape(rawText)))]
    const close = unspaced(Token.new("'"))
    const base = module.baseObject('TextLiteral')
    const fields = setAll(base, {
      open,
      newline: null,
      elements,
      close,
    })
    return asOwned(new TextLiteral(module, fields))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, newline, elements, close } = getAll(this.fields)
    if (open) yield open
    if (newline) yield newline
    yield* elements
    if (close) yield close
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const replacement_ = autospaced(this.claimChild(replacement))
    this.fields.set(
      'elements',
      this.fields.get('elements').map((child) => (child.node === target ? replacement_ : child)),
    )
  }
}

type DocumentedFields = {
  open: NodeChild<Token> | null
  elements: NodeChild[]
  newlines: NodeChild<Token>[]
  expression: NodeChild<AstId> | null
}
export class Documented extends Ast {
  declare fields: FixedMap<AstFields & DocumentedFields>
  constructor(module: Module, fields: FixedMap<AstFields & DocumentedFields>) {
    super(module, fields)
  }

  get expression(): Ast | null {
    return this.module.get(this.fields.get('expression')?.node ?? null)
  }
  setExpression<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('expression', value ? unspaced(this.claimChild(value)) : null)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, elements, newlines, expression } = getAll(this.fields)
    if (open) yield open
    yield* elements
    yield* newlines
    if (expression) yield expression
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
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

type InvalidFields = { expression: NodeChild<AstId> }
export class Invalid extends Ast {
  declare fields: FixedMap<AstFields & InvalidFields>
  constructor(module: Module, fields: FixedMap<AstFields & InvalidFields>) {
    super(module, fields)
  }

  get expression(): Ast {
    return this.module.get(this.fields.get('expression').node)
  }
  setExpression<T extends Ast>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('expression')
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    assertEqual(this.fields.get('expression').node, target)
    this.setExpression(replacement)
  }
}

type GroupFields = {
  open: NodeChild<Token>
  expression: NodeChild<AstId> | null
  close: NodeChild<Token>
}
export class Group extends Ast {
  declare fields: FixedMap<AstFields & GroupFields>
  constructor(module: Module, fields: FixedMap<AstFields & GroupFields>) {
    super(module, fields)
  }

  get expression(): Ast | null {
    return this.module.get(this.fields.get('expression')?.node ?? null)
  }
  setExpression<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('expression', value ? unspaced(this.claimChild(value)) : null)
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { open, expression, close } = getAll(this.fields)
    yield open
    if (expression) yield expression
    yield close
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    assertEqual(this.fields.get('expression')?.node, target)
    this.setExpression(replacement)
  }
}

type NumericLiteralFields = {
  tokens: NodeChild[]
}
export class NumericLiteral extends Ast {
  declare fields: FixedMap<AstFields & NumericLiteralFields>
  constructor(module: Module, fields: FixedMap<AstFields & NumericLiteralFields>) {
    super(module, fields)
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.fields.get('tokens')[Symbol.iterator]()
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const replacement_ = autospaced(this.claimChild(replacement))
    this.fields.set(
      'tokens',
      this.fields.get('tokens').map((child) => (child.node === target ? replacement_ : child)),
    )
  }
}

/** The actual contents of an `ArgumentDefinition` are complex, but probably of more interest to the compiler than the
 *  GUI. We just need to represent them faithfully and create the simple cases. */
type ArgumentDefinition = NodeChild<Ast | Token>[]
type RawArgumentDefinition = NodeChild[]
type OwnedArgumentDefinition = NodeChild<Owned<Ast> | Token>[]

function argumentDefinitionsToRaw(
  module: Module,
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
  equals: NodeChild<Token>
  body: NodeChild<AstId> | null
}
export class Function extends Ast {
  declare fields: FixedMap<AstFields & FunctionFields>
  constructor(module: Module, fields: FixedMap<AstFields & FunctionFields>) {
    super(module, fields)
  }

  get name(): Ast {
    return this.module.get(this.fields.get('name').node)
  }
  get body(): Ast | null {
    return this.module.get(this.fields.get('body')?.node ?? null)
  }
  get argumentDefinitions(): ArgumentDefinition[] {
    return this.fields
      .get('argumentDefinitions')
      .map((raw) =>
        raw.map((part) => ({
          ...part,
          node: part.node instanceof Token ? part.node : this.module.get(part.node),
        })),
      )
  }
  setName<T extends Ast>(value: Owned<T>) {
    this.fields.set('name', unspaced(this.claimChild(value)))
  }
  setBody<T extends Ast>(value: Owned<T> | null) {
    this.fields.set('body', value ? unspaced(this.claimChild(value)) : null)
  }
  setArgumentDefinitions(defs: OwnedArgumentDefinition[]) {
    this.fields.set('argumentDefinitions', argumentDefinitionsToRaw(this.module, defs, this.exprId))
  }

  static new(
    module: Module,
    name: Token | string,
    argumentDefinitions: OwnedArgumentDefinition[],
    body: Owned<BodyBlock>,
  ): Owned<Function> {
    const base = module.baseObject('Function')
    const id = base.get('id')
    const bodyId = claimChild(module, body, id)
    const path = claimChild(module, PropertyAccess.Sequence([name], module), id)
    const fields = setAll(base, {
      name: unspaced(path),
      argumentDefinitions: argumentDefinitionsToRaw(module, argumentDefinitions, id),
      equals: spaced(makeEquals()),
      body: bodyId ? autospaced(bodyId) : null,
    })
    return asOwned(new Function(module, fields))
  }

  /** Construct a function with simple (name-only) arguments and a body block. */
  static fromStatements(
    module: Module,
    name: Token | string,
    argumentNames: (Token | string)[],
    statements: Owned<Ast>[],
    trailingNewline?: boolean,
  ): Owned<Function> {
    const statements_: OwnedBlockLine[] = statements.map((statement) => ({
      expression: unspaced(statement),
    }))
    if (trailingNewline) {
      statements_.push({ expression: null })
    }
    const argumentDefinitions = argumentNames.map((name) => [unspaced(Ident.new(module, name))])
    const body = BodyBlock.new(statements_, module)
    return Function.new(module, name, argumentDefinitions, body)
  }

  *bodyExpressions(): IterableIterator<Ast> {
    const body = this.body
    if (body instanceof BodyBlock) {
      yield* body.statements()
    } else if (body !== null) {
      yield body
    }
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const { name, argumentDefinitions, equals, body } = getAll(this.fields)
    yield name
    for (const def of argumentDefinitions) yield* def
    yield { whitespace: equals.whitespace ?? ' ', node: equals.node }
    if (body) yield body
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const { name, argumentDefinitions, equals, body } = getAll(this.fields)
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

type AssignmentFields = {
  pattern: NodeChild<AstId>
  equals: NodeChild<Token>
  expression: NodeChild<AstId>
}
export class Assignment extends Ast {
  declare fields: FixedMap<AstFields & AssignmentFields>
  constructor(module: Module, fields: FixedMap<AstFields & AssignmentFields>) {
    super(module, fields)
  }

  static new(module: Module, ident: string | Token, expression: Owned<Ast>) {
    const base = module.baseObject('Assignment')
    const id = base.get('id')
    const fields = setAll(base, {
      pattern: unspaced(claimChild(module, Ident.new(module, ident), id)),
      equals: spaced(makeEquals()),
      expression: spaced(claimChild(module, expression, id)),
    })
    return asOwned(new Assignment(module, fields))
  }

  get pattern(): Ast {
    return this.module.get(this.fields.get('pattern').node)
  }
  get expression(): Ast {
    return this.module.get(this.fields.get('expression').node)
  }
  setPattern<T extends Ast>(value: Owned<T>) {
    this.fields.set('pattern', unspaced(this.claimChild(value)))
  }
  setExpression<T extends Ast>(value: Owned<T>) {
    this.fields.set('expression', unspaced(this.claimChild(value)))
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

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const { pattern, expression } = getAll(this.fields)
    if (pattern.node === target) {
      this.setPattern(replacement)
    } else if (expression.node === target) {
      this.setExpression(replacement)
    }
  }
}

type BodyBlockFields = {
  lines: RawBlockLine[]
}
export class BodyBlock extends Ast {
  declare fields: FixedMap<AstFields & BodyBlockFields>
  constructor(module: Module, fields: FixedMap<AstFields & BodyBlockFields>) {
    super(module, fields)
  }

  static new(lines: OwnedBlockLine[], module: Module) {
    const base = module.baseObject('BodyBlock')
    const id = base.get('id')
    const fields = setAll(base, {
      lines: lines.map((line) => lineToRaw(line, module, id)),
    })
    return asOwned(new BodyBlock(module, fields))
  }

  get lines(): BlockLine[] {
    return this.fields.get('lines').map((line) => lineFromRaw(line, this.module))
  }
  takeLines(edit: Module): OwnedBlockLine[] {
    return this.fields.get('lines').map((line) => ownedLineFromRaw(line, edit))
  }

  *statements(): IterableIterator<Ast> {
    for (const line of this.lines) {
      if (line.expression) yield line.expression.node
    }
  }

  /** Insert the given statement(s) starting at the specified line index. */
  insert(module: Module, index: number, ...statements: Owned<Ast>[]) {
    const before = this.lines.slice(0, index)
    const insertions = statements.map((statement) => ({
      expression: unspaced(claimChild(module, statement, this.exprId)),
    }))
    const after = this.lines.slice(index)
    throw new Error('TODO')
    //const edited = new BodyBlock(module, this.exprId, [...before, ...insertions, ...after])
    //edited.parent = this.parentId
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    for (const line of this.fields.get('lines')) {
      yield line.newline ?? { node: new Token('\n', newTokenId(), RawAst.Token.Type.Newline) }
      if (line.expression !== null) yield line.expression
    }
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {
    const replacement_ = this.claimChild(replacement)
    const updateLine = (line: RawBlockLine) => (line.expression?.node === target) ? { ...line, expression: { ...line.expression, node: replacement_ } } : line
    this.fields.set('lines', this.fields.get('lines').map(updateLine))
  }

  printSubtree(info: InfoMap, offset: number, parentIndent: string | null): string {
    let blockIndent: string | undefined
    let code = ''
    for (const line of this.fields.get('lines')) {
      code += line.newline?.whitespace ?? ''
      const newlineCode = line.newline?.node.code()
      // Only print a newline if this isn't the first line in the output, or it's a comment.
      if (offset || code || newlineCode?.startsWith('#')) {
        // If this isn't the first line in the output, but there is a concrete newline token:
        // if it's a zero-length newline, ignore it and print a normal newline.
        code += newlineCode || '\n'
      }
      if (line.expression !== null) {
        if (blockIndent === undefined) {
          if ((line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)) {
            blockIndent = line.expression.whitespace!
          } else if (parentIndent !== null) {
            blockIndent = parentIndent + '    '
          } else {
            blockIndent = ''
          }
        }
        const validIndent = (line.expression.whitespace?.length ?? 0) > (parentIndent?.length ?? 0)
        code += validIndent ? line.expression.whitespace : blockIndent
        const lineNode = this.module.get(line.expression.node)
        assertEqual(lineNode.exprId, line.expression.node)
        assertEqual(lineNode.parentId, this.exprId)
        code += lineNode.printSubtree(info, offset + code.length, blockIndent)
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this.exprId])
    } else {
      infos.unshift(this.exprId)
    }
    return code
  }
}

type Line<T> = {
  newline?: NodeChild<Token> | undefined
  expression: NodeChild<T> | null
}

type RawBlockLine = Line<AstId>
type BlockLine = Line<Ast>
type OwnedBlockLine = Line<Owned<Ast>>

function lineFromRaw(raw: RawBlockLine, module: Module): BlockLine {
  const expression = raw.expression ? module.get(raw.expression.node) : null
  return {
    newline: raw.newline,
    expression: expression
      ? {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : null,
  }
}

function ownedLineFromRaw(raw: RawBlockLine, module: Module): OwnedBlockLine {
  const expression = raw.expression ? module.take(raw.expression.node)!.node : null
  return {
    newline: raw.newline,
    expression: expression
      ? {
          whitespace: raw.expression?.whitespace,
          node: expression,
        }
      : null,
  }
}

function lineToRaw(line: OwnedBlockLine, module: Module, block: AstId): RawBlockLine {
  return {
    newline: line.newline,
    expression: line.expression
      ? {
          whitespace: line.expression?.whitespace,
          node: claimChild(module, line.expression.node, block),
        }
      : null,
  }
}

type IdentFields = {
  token: NodeChild<Token>
}
export class Ident extends Ast {
  declare fields: FixedMap<AstFields & IdentFields>
  constructor(module: Module, fields: FixedMap<AstFields & IdentFields>) {
    super(module, fields)
  }

  get token(): Token {
    return this.fields.get('token').node
  }
  setToken(ident: Token | string) {
    this.fields.set('token', unspaced(toIdent(ident)))
  }

  static new(module: Module, ident: Token | string) {
    const base = module.baseObject('Ident')
    const fields = setAll(base, {
      token: unspaced(toIdent(ident)),
    })
    return asOwned(new Ident(module, fields))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {}
}

type WildcardFields = {
  token: NodeChild<Token>
}
export class Wildcard extends Ast {
  declare fields: FixedMap<AstFields & WildcardFields>
  constructor(module: Module, fields: FixedMap<AstFields & WildcardFields>) {
    super(module, fields)
  }

  get token(): Token {
    return this.fields.get('token').node
  }

  static new(module: Module) {
    const base = module.baseObject('Wildcard')
    const fields = setAll(base, {
      token: unspaced(new Token('_', newTokenId(), RawAst.Token.Type.Wildcard)),
    })
    return asOwned(new Wildcard(module, fields))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.fields.get('token')
  }

  replaceChild<T extends Ast>(target: AstId, replacement: Owned<T>) {}
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
export function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v"'`]/g, (match) => mapping[match]!)
}

function abstract(
  module: Module,
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
  module: Module,
  tree: RawAst.Tree,
  code: string,
  nodesExpected: NodeSpanMap,
  tokens: TokenSpanMap,
  tokensOut: TokenSpanMap,
): { whitespace: string | undefined; node: Ast } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesExpected, tokens, tokensOut)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokens, tokensOut)
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild<Ast | Token>[] = []
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
  const spanKey = nodeKey(codeStart, codeEnd - codeStart, tree.type)
  let node: AstId
  switch (tree.type) {
    case RawAst.Tree.Type.BodyBlock: {
      const lines = Array.from(tree.statements, (line) => {
        const newline = recurseToken(line.newline)
        let expression = line.expression ? recurseTree(line.expression) : null
        return {
          newline,
          expression: expression ? { ...expression, node: expression.node.exprId } : null,
        }
      })
      const id = nodesExpected.get(spanKey)?.pop()
      node = new BodyBlock(module, id, lines).exprId
      break
    }
    case RawAst.Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body = tree.body !== undefined ? recurseTree(tree.body) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Function(module, id, name, args, equals, body).exprId
      break
    }
    case RawAst.Tree.Type.Ident: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Ident(module, id, recurseToken(tree.token)).exprId
      break
    }
    case RawAst.Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Assignment(module, id, pattern, equals, value).exprId
      break
    }
    case RawAst.Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(module, id, func, arg).exprId
      break
    }
    case RawAst.Tree.Type.NamedApp: {
      const func = recurseTree(tree.func)
      const leftParen = tree.open ? recurseToken(tree.open) : null
      const name = recurseToken(tree.name)
      const equals = recurseToken(tree.equals)
      const arg = recurseTree(tree.arg)
      const rightParen = tree.close ? recurseToken(tree.close) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = namedApp(module, id, func, leftParen, name, equals, arg, rightParen).exprId
      break
    }
    case RawAst.Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.node.code() === '-') {
        node = new NegationOprApp(module, id, opr, arg).exprId
      } else {
        node = new UnaryOprApp(module, id, opr, arg).exprId
      }
      break
    }
    case RawAst.Tree.Type.OprApp: {
      const lhs = tree.lhs ? recurseTree(tree.lhs) : null
      const opr = tree.opr.ok
        ? [recurseToken(tree.opr.value)]
        : visitChildren(tree.opr.error.payload)
      const rhs = tree.rhs ? recurseTree(tree.rhs) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.length === 1 && opr[0]?.node instanceof Token && opr[0].node.code() === '.') {
        // Propagate inferred type.
        const token = { whitespace: opr[0].whitespace, node: opr[0].node }
        node = new PropertyAccess(module, id, lhs, token, rhs).exprId
      } else {
        node = new OprApp(module, id, lhs, opr, rhs).exprId
      }
      break
    }
    case RawAst.Tree.Type.Number: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new NumericLiteral(module, id, visitChildren(tree)).exprId
      break
    }
    case RawAst.Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Wildcard(module, id, token).exprId
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
      node = new Invalid(module, id, expression).exprId
      break
    }
    case RawAst.Tree.Type.Group: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const expression = tree.body ? recurseTree(tree.body) : null
      const close = tree.close ? recurseToken(tree.close) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Group(module, id, open, expression, close).exprId
      break
    }
    case RawAst.Tree.Type.TextLiteral: {
      const open = tree.open ? recurseToken(tree.open) : null
      const newline = tree.newline ? recurseToken(tree.newline) : null
      const elements = []
      for (const e of tree.elements) {
        elements.push(...visitChildren(e))
      }
      const close = tree.close ? recurseToken(tree.close) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new TextLiteral(module, id, open, newline, elements, close).exprId
      break
    }
    case RawAst.Tree.Type.Documented: {
      const open = recurseToken(tree.documentation.open)
      const elements = []
      for (const e of tree.documentation.elements) {
        elements.push(...visitChildren(e))
      }
      const newlines = Array.from(tree.documentation.newlines, recurseToken)
      const id = nodesExpected.get(spanKey)?.pop()
      const expression = tree.expression ? recurseTree(tree.expression) : null
      node = new Documented(module, id, open, elements, newlines, expression).exprId
      break
    }
    case RawAst.Tree.Type.Import: {
      const recurseBody = (tree: RawAst.Tree) => {
        const body = recurseTree(tree)
        const bodyAst = module.get(body.node)
        if (bodyAst instanceof Invalid && bodyAst.code() === '') return null
        return body
      }
      const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
        header: recurseToken(segment.header),
        body: segment.body ? recurseBody(segment.body) : null,
      })
      const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : null
      const from = tree.from ? recurseSegment(tree.from) : null
      const import_ = recurseSegment(tree.import)
      const all = tree.all ? recurseToken(tree.all) : null
      const as = tree.as ? recurseSegment(tree.as) : null
      const hiding = tree.hiding ? recurseSegment(tree.hiding) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Import(module, id, polyglot, from, import_, all, as, hiding).exprId
      break
    }
    default: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Generic(module, id, visitChildren(tree), tree.type).exprId
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
function nodeKey(start: number, length: number, type: Opt<RawAst.Tree.Type>): NodeKey {
  const type_ = type?.toString() ?? '?'
  return `${start}:${length}:${type_}` as NodeKey
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
export function print(ast: AstId, module: Module): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
    tokensOut: new Map(),
  }
  const code = module.get(ast)!.printSubtree(info, 0, null, module)
  return { info, code }
}

export type TokenTree = (TokenTree | string)[]
export function tokenTree(root: Ast): TokenTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (child.node instanceof Token) {
      return child.node.code()
    } else {
      const node = module.get(child.node)
      return node ? tokenTree(node) : '<missing>'
    }
  })
}

export function tokenTreeWithIds(root: Ast): TokenTree {
  const module = root.module
  return [
    root.exprId,
    ...Array.from(root.concreteChildren(), (child) => {
      if (child.node instanceof Token) {
        return child.node.code()
      } else {
        const node = module.get(child.node)
        return node ? tokenTreeWithIds(node) : ['<missing>']
      }
    }),
  ]
}

export function moduleMethodNames(module: Module): Set<string> {
  const result = new Set<string>()
  for (const node of module.iterAllNodes()) {
    if (node instanceof Function && node.name) {
      result.add(node.name.code())
    }
  }
  return result
}

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(module: Module, name: string): Function | null {
  for (const node of module.iterAllNodes()) {
    if (node instanceof Function) {
      if (node.name && node.name.code() === name) {
        return node
      }
    }
  }
  return null
}

export function functionBlock(module: Module, name: string): BodyBlock | null {
  const method = findModuleMethod(module, name)
  if (!method || !(method.body instanceof BodyBlock)) return null
  return method.body
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
      const key = nodeKey(start, length, node.inner.type)
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
  const newRoot = Ast.parseBlock({ info: { nodes, tokens, tokensOut }, code })
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
  const printed = print(newRoot.exprId, newRoot.module)
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

export const parseBlock = Ast.parseBlock
export const parse = Ast.parse

export function deserialize(serialized: string): Ast {
  return Ast.deserialize(serialized)
}

declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [TokenKey]: Token
  }
}
