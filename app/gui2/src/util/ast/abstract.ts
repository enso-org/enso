import * as RawAst from '@/generated/ast'
import { assert, assertEqual } from '@/util/assert'
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
import type { ExternalId, SourceRange, Uuid } from '../../../shared/yjsModel'
import { IdMap } from '../../../shared/yjsModel'

declare const brandOwned: unique symbol
/** Used to mark references required to be unique.
 *
 *  Note that the typesystem cannot stop you from copying an `Owned`,
 *  but that is an easy mistake to see (because it occurs locally).
 *
 *  We can at least require *obtaining* an `Owned`,
 *  which statically prevents the otherwise most likely usage errors when rearranging ASTs.
 */
export type Owned<T = Ast> = T & { [brandOwned]: never }
function asOwned<T>(t: T): Owned<T> {
  return t as Owned<T>
}

export interface Module {
  get raw(): MutableModule
  get(id: AstId): Ast | null
  getExtended(id: AstId): RawAstExtended | undefined
  edit(): MutableModule
  apply(module: Module): void
  replace(editIn: Module): void
  iterAllNodeIds(): IterableIterator<AstId>
  iterAllNodes(): IterableIterator<Ast>
  root(): Module
  has(id: AstId): boolean
}

export class MutableModule implements Module {
  readonly base: Module | null
  readonly nodes: Map<AstId, Ast | null>
  astExtended: Map<AstId, RawAstExtended> | null
  spans: Map<AstId, SourceRange> | null

  constructor(
    base: Module | null,
    nodes: Map<AstId, Ast | null>,
    astExtended: Map<AstId, RawAstExtended> | null,
    spans: Map<AstId, SourceRange> | null,
  ) {
    this.base = base
    this.nodes = nodes
    this.astExtended = astExtended
    this.spans = spans
  }

  static Observable(): MutableModule {
    const nodes = reactive(new Map())
    const astExtended = reactive(new Map())
    const spans = reactive(new Map())
    return new MutableModule(null, nodes, astExtended, spans)
  }

  static Transient(base?: Module): MutableModule {
    const nodes = new Map<AstId, Ast>()
    return new MutableModule(base ?? null, nodes, null, null)
  }

  edit(): MutableModule {
    return MutableModule.Transient(this)
  }

  get raw(): MutableModule {
    return this
  }

  apply(editIn: Module) {
    const edit = editIn.raw
    if (edit.astExtended) {
      const astExtended = this.astExtended ?? new Map()
      for (const [id, ast] of edit.astExtended.entries()) {
        astExtended.set(id, ast)
      }
      this.astExtended = astExtended
    }
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
        this.astExtended?.delete(id)
        this.spans?.delete(id)
      } else {
        this.nodes.set(id, ast)
      }
    }
  }

  /** Replace the contents of this module with the contents of the specified module. */
  replace(editIn: Module) {
    const edit = editIn.raw
    for (const id of this.nodes.keys()) {
      if (!edit.nodes.has(id)) {
        this.nodes.delete(id)
        this.astExtended?.delete(id)
        this.spans?.delete(id)
      }
    }
    this.apply(edit)
  }

  /** Returns a syntax node representing the current committed state of the given ID. */
  get(id: AstId): Ast | null {
    const editedNode = this.nodes.get(id)
    if (editedNode === null) {
      return null
    } else {
      return editedNode ?? this.base?.get(id) ?? null
    }
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

  /** Modify the parent of `target` to refer to a new object instead of `target`. Return `target`, which now has no parent. */
  replaceRef<T extends Ast>(target: AstId, replacement: Owned<T>): Owned | undefined {
    const old = this.get(target)
    if (!old || replacement.id === target) {
      return this.replaceValue(target, replacement)
    } else {
      assert(!!old.parent)
      const parentSomewhere = this.get(old.parent)
      assert(!!parentSomewhere)
      const parent = this.spliceShallow(parentSomewhere)
      let foundChildInParent = false
      for (const child of parent.concreteChildren()) {
        if (child.node === target) {
          child.node = makeChild(this, replacement, parent.id)
          foundChildInParent = true
          break
        }
      }
      assert(foundChildInParent)
      for (const child of parent.concreteChildren()) assert(child.node !== target)
      const old_ = this.spliceShallow(old)
      old_.parent = undefined
      return asOwned(old_)
    }
  }

  /** Change the value of the object referred to by the `target` ID. (The initial ID of `replacement` will be ignored.)
   *  Returns the old value, with a new (unreferenced) ID.
   */
  replaceValue<T extends Ast>(target: AstId, replacement: Owned<T>): Owned | undefined {
    const old = this.get(target)
    replacement.parent = old?.parent
    if (replacement.id !== target || replacement.module !== this) {
      this.splice(replacement, target)
    } else {
      this.nodes.set(target, replacement)
    }
    if (replacement.module === this && replacement.id !== target) this.nodes.delete(replacement.id)
    if (old) {
      const old_ = this.splice(old, newAstId())
      old_.parent = undefined
      return asOwned(old_)
    } else {
      return undefined
    }
  }

  /** Replace the parent of `target`'s reference to it with a reference to a new placeholder object. Return `target`. */
  take(target: AstId): { node: Owned; placeholder: Wildcard } | undefined {
    const placeholder = Wildcard.new(this)
    const node = this.replaceRef(target, placeholder)
    if (!node) return
    return { node, placeholder }
  }

  /** Replace the value assigned to the given ID with a placeholder.
   *  Returns the removed value, with a new unreferenced ID.
   **/
  takeValue(target: AstId): Owned | undefined {
    return this.replaceValue(target, Wildcard.new(this))
  }

  takeAndReplaceRef<T extends Ast>(target: AstId, wrap: (x: Owned) => Owned<T>): T {
    const taken = this.take(target)
    assert(!!taken)
    const replacement = wrap(taken.node)
    this.replaceRef(taken.placeholder.id, replacement)
    return replacement
  }

  takeAndReplaceValue<T extends Ast>(target: AstId, wrap: (x: Owned) => Owned<T>): T {
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
    const id_ = id ?? ast.id
    if (ast.module === this && this.nodes.get(id_) === ast) return ast
    const ast_ = ast.cloneWithId(this, id_)
    for (const child of ast_.concreteChildren()) {
      if (!(child.node instanceof Token)) {
        const childInForeignModule = ast.module.get(child.node)
        assert(childInForeignModule !== null)
        const child_ = this.splice(childInForeignModule)
        child_.parent = id_
      }
    }
    this.nodes.set(id_, ast_)
    return ast_
  }

  spliceShallow(ast: Ast): Ast {
    const cloned = ast.cloneWithId(this, ast.id)
    this.nodes.set(ast.id, cloned)
    return cloned
  }

  getExtended(id: AstId): RawAstExtended | undefined {
    return this.astExtended?.get(id) ?? this.base?.getExtended(id)
  }

  /** Remove the expression with the specified ID.
   *
   *  If the expression is optional in its parent, it will be removed entirely.
   *  E.g. if it is a direct child of a `BodyBlock`, its line will be eliminated.
   *
   *  If the expression is a required part of the structure of its parent, it will be replaced with a placeholder.
   *  In most contexts within an expression, this will be `_`.
   */
  delete(id: AstId) {
    this.nodes.set(id, null)
  }

  root(): Module {
    if (!this.base) return this
    return this.base.root()
  }

  has(id: AstId) {
    return this.nodes.has(id) || (this.base ? this.base.has(id) : false)
  }
}

export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn.id, rootIn.module)
  const module = MutableModule.Transient()
  const tree = parseEnso(printed.code)
  const rootOut = abstract(module, tree, printed.code, printed.info).node
  return module.get(rootOut)!
}

export type NodeChild<T = AstId | Token> = { whitespace?: string | undefined; node: T }

declare const brandAstId: unique symbol
declare const brandTokenId: unique symbol
export type AstId = Uuid & { [brandAstId]: never }
export type TokenId = Uuid & { [brandTokenId]: never }

function newAstId(): AstId {
  return random.uuidv4() as AstId
}
function newTokenId(): TokenId {
  return random.uuidv4() as TokenId
}
// Cast an ID which may be a tree or a token to a tree ID.
export function asAstId(expr: AstId | TokenId): AstId {
  return expr as AstId
}

export class Token {
  readonly id: TokenId
  code_: string
  tokenType_: RawAst.Token.Type | undefined
  constructor(code: string, id: TokenId, type: RawAst.Token.Type | undefined) {
    this.id = id
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

export abstract class Ast {
  readonly treeType: RawAst.Tree.Type | undefined
  readonly id: AstId
  readonly module: Module
  parent: AstId | undefined

  get externalId(): ExternalId {
    return this.id as Uuid as ExternalId
  }

  // Deprecated interface for incremental integration of Ast API. Eliminate usages for #8367.
  get astExtended(): RawAstExtended | undefined {
    return this.module.getExtended(this.id)
  }

  serialize(): string {
    return JSON.stringify(print(this.id, this.module))
  }

  static deserialize(serialized: string): Ast {
    const parsed: SerializedPrintedSource = JSON.parse(serialized)
    const nodes = new Map(unsafeEntries(parsed.info.nodes))
    const tokens = new Map(unsafeEntries(parsed.info.tokens))
    const tokensOut = new Map()
    const module = MutableModule.Transient()
    const tree = parseEnso(parsed.code)
    const root = abstract(module, tree, parsed.code, { nodes, tokens, tokensOut }).node
    return module.get(root)!
  }

  /** Return this node's span, if it belongs to a module with an associated span map. */
  get span(): SourceRange | undefined {
    const spans = this.module.raw.spans
    if (spans) return spans.get(this.id)
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

  /** Returns child subtrees, including information about the whitespace between them. */
  abstract concreteChildren(): IterableIterator<NodeChild>

  code(module?: Module): string {
    return print(this.id, module ?? this.module).code
  }

  typeName(): string | undefined {
    if (this.treeType === undefined) return undefined
    return RawAst.Tree.typeNames[this.treeType]
  }

  /** Parse the input as a block. */
  static parseBlock(source: PrintedSource | string, inModule?: MutableModule | undefined) {
    const code = typeof source === 'object' ? source.code : source
    const ids = typeof source === 'object' ? source.info : undefined
    const tree = parseEnso(code)
    const module = inModule ?? MutableModule.Transient()
    const newRoot = abstract(module, tree, code, ids).node
    const ast = module.get(newRoot)
    // The root of the tree produced by the parser is always a `BodyBlock`.
    const block = ast as BodyBlock
    return asOwned(block)
  }

  /** Parse the input. If it contains a single expression at the top level, return it; otherwise, return a block. */
  static parse(source: PrintedSource | string, module?: MutableModule): Owned {
    const module_ = module ?? MutableModule.Transient()
    const ast = Ast.parseBlock(source, module_)
    const [expr] = ast.statements()
    if (!expr) return ast
    if (expr.parent) module_.nodes.delete(expr.parent)
    expr.parent = undefined
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

  protected constructor(module: MutableModule, id?: AstId, treeType?: RawAst.Tree.Type) {
    this.module = module
    this.id = id ?? newAstId()
    this.treeType = treeType
    module.nodes.set(this.id, this)
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
        info.tokens.set(span, child.node.id)
        code += tokenCode
      } else {
        const childNode = module_.get(child.node)
        assert(childNode != null)
        code += childNode.printSubtree(info, offset + code.length, parentIndent, moduleOverride)
        // Extra structural validation.
        assertEqual(childNode.id, child.node)
        if (childNode.parent !== this.id) {
          console.error(`Inconsistent parent pointer (expected ${this.id})`, childNode)
        }
        assertEqual(childNode.parent, this.id)
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = map.setIfUndefined(info.nodes, span, (): AstId[] => [])
    infos.unshift(this.id)
    return code
  }

  /** Return a copy of this node, with the specified `module` and `id` properties.
   *
   *  The node's owned data is deep-copied, although note that child subtrees are stored as IDs,
   *  so a full deep copy requires recursively cloning child nodes.
   */
  cloneWithId(module: Module, id: AstId): this {
    const cloned = clone(this)
    // No one else has a reference to this object, so we can ignore `readonly` and mutate it.
    Object.assign(cloned, { module, id })
    return cloned
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

function makeChild<T extends Ast>(module: MutableModule, child: Owned<T>, parent: AstId): AstId {
  assert(
    !child.parent || !module.has(child.parent),
    'Owned object is not owned. Was it obtained from a different module?',
  )
  const spliced = module.splice(child)
  spliced.parent = parent
  return spliced.id
}

function setParent(module: MutableModule, parent: AstId, ...children: (NodeChild | null)[]) {
  for (const child of children) {
    if (child && !(child.node instanceof Token)) module.get(child.node)!.parent = parent
  }
}

export class App extends Ast {
  private readonly func_: NodeChild<AstId>
  private readonly leftParen_: NodeChild<Token> | null
  private readonly argumentName_: NodeChild<Token> | null
  private readonly equals_: NodeChild<Token> | null
  private readonly arg_: NodeChild<AstId>
  private readonly rightParen_: NodeChild<Token> | null

  static new(func: Owned, name: string | Token | null, arg: Owned, module?: MutableModule) {
    const edit = module ?? MutableModule.Transient()
    const id = newAstId()
    const func_ = unspaced(makeChild(edit, func, id))
    const nameToken =
      typeof name === 'string' ? new Token(name, newTokenId(), RawAst.Token.Type.Ident) : null
    const name_ = nameToken ? spaced(nameToken) : null
    const equals = name ? unspaced(new Token('=', newTokenId(), RawAst.Token.Type.Operator)) : null
    const arg_ = spacedIf(makeChild(edit, arg, id), !name)
    const treeType = name ? RawAst.Tree.Type.NamedApp : RawAst.Tree.Type.App
    return asOwned(new App(edit, id, func_, null, name_, equals, arg_, null, treeType))
  }

  get function(): Ast {
    return this.module.get(this.func_.node)!
  }

  get argumentName(): Token | null {
    return this.argumentName_?.node ?? null
  }

  get argument(): Ast {
    return this.module.get(this.arg_.node)!
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    func: NodeChild<AstId>,
    leftParen: NodeChild<Token> | null,
    name: NodeChild<Token> | null,
    equals: NodeChild<Token> | null,
    arg: NodeChild<AstId>,
    rightParen: NodeChild<Token> | null,
    treeType: RawAst.Tree.Type,
  ) {
    super(module, id, treeType)
    setParent(module, this.id, func, arg)
    this.func_ = func
    this.leftParen_ = leftParen
    this.argumentName_ = name
    this.equals_ = equals
    this.arg_ = arg
    this.rightParen_ = rightParen
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.func_
    if (this.leftParen_) yield this.leftParen_
    if (this.argumentName_) yield this.argumentName_
    if (this.equals_)
      yield { whitespace: this.equals_.whitespace ?? this.arg_.whitespace, node: this.equals_.node }
    yield this.arg_
    if (this.rightParen_) yield this.rightParen_
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
export function escape(string: string) {
  return string.replace(/[\0\b\f\n\r\t\v"'`]/g, (match) => mapping[match]!)
}

function positionalApp(
  module: MutableModule,
  id: AstId | undefined,
  func: NodeChild<AstId>,
  arg: NodeChild<AstId>,
): App {
  return new App(module, id, func, null, null, null, arg, null, RawAst.Tree.Type.App)
}

function namedApp(
  module: MutableModule,
  id: AstId | undefined,
  func: NodeChild<AstId>,
  leftParen: NodeChild<Token> | null,
  name: NodeChild<Token>,
  equals: NodeChild<Token>, // Edits (#8367): NodeChild<Tok> | undefined
  arg: NodeChild<AstId>,
  rightParen: NodeChild<Token> | null,
) {
  return asOwned(
    new App(module, id, func, leftParen, name, equals, arg, rightParen, RawAst.Tree.Type.NamedApp),
  )
}

export class UnaryOprApp extends Ast {
  private readonly opr: NodeChild<Token>
  private readonly arg: NodeChild<AstId> | null

  get operator(): Token {
    return this.opr.node
  }

  get argument(): Ast | null {
    const id = this.arg?.node
    return id ? this.module.get(id) : null
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    opr: NodeChild<Token>,
    arg: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.UnaryOprApp)
    setParent(module, this.id, arg)
    this.opr = opr
    this.arg = arg
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.opr
    if (this.arg) yield this.arg
  }
}

export class NegationOprApp extends UnaryOprApp {
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    opr: NodeChild<Token>,
    arg: NodeChild<AstId> | null,
  ) {
    super(module, id, opr, arg)
  }
}

export class OprApp extends Ast {
  private readonly lhs_: NodeChild<AstId> | null
  private readonly opr_: NodeChild[]
  private readonly rhs_: NodeChild<AstId> | null

  get lhs(): Ast | null {
    return this.lhs_ ? this.module.get(this.lhs_.node) : null
  }

  get operator(): Result<Token, NodeChild[]> {
    const first = this.opr_[0]?.node
    if (first && this.opr_.length < 2 && first instanceof Token) {
      return Ok(first)
    } else {
      return Err(this.opr_)
    }
  }

  get rhs(): Ast | null {
    return this.rhs_ ? this.module.get(this.rhs_.node) : null
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild[],
    rhs: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.OprApp)
    setParent(module, this.id, lhs, rhs)
    this.lhs_ = lhs
    this.opr_ = opr
    this.rhs_ = rhs
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this.lhs_) yield this.lhs_
    for (const opr of this.opr_) yield opr
    if (this.rhs_) yield this.rhs_
  }
}

export class PropertyAccess extends OprApp {
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild<Token> | undefined,
    rhs: NodeChild<AstId> | null,
  ) {
    const oprs = [opr ?? unspaced(new Token('.', newTokenId(), RawAst.Token.Type.Operator))]
    super(module, id, lhs, oprs, rhs)
  }

  static new(module: MutableModule, lhs: Owned | null, rhs: Token | null) {
    const id = newAstId()
    const lhs_ = lhs ? unspaced(makeChild(module, lhs, id)) : null
    let rhs_ = null
    if (rhs) {
      const ident = asOwned(new Ident(module, undefined, unspaced(rhs)))
      rhs_ = unspaced(makeChild(module, ident, id))
    }
    return asOwned(new PropertyAccess(module, id, lhs_, undefined, rhs_))
  }

  static Sequence(
    segments: string[],
    module?: MutableModule,
  ): Owned<PropertyAccess> | Owned<Ident> | undefined {
    const module_ = module ?? MutableModule.Transient()
    let path
    for (const s of segments) {
      const t = new Token(s, newTokenId(), RawAst.Token.Type.Ident)
      if (!path) {
        path = Ident.new(module_, s)
        continue
      }
      path = PropertyAccess.new(module_, path, t)
    }
    return path
  }
}

/** Representation without any type-specific accessors, for tree types that don't require any special treatment. */
export class Generic extends Ast {
  private readonly children_: NodeChild[]

  constructor(
    module: MutableModule,
    id?: AstId,
    children?: NodeChild[],
    treeType?: RawAst.Tree.Type,
  ) {
    super(module, id, treeType)
    this.children_ = children ?? []
    setParent(module, this.id, ...this.children_)
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.children_.values()
  }
}

type MultiSegmentAppSegment = { header: NodeChild<Token>; body: NodeChild<AstId> | null }
function multiSegmentAppSegment(
  whitespace: string,
  header: string,
  body: Ast,
): MultiSegmentAppSegment {
  return {
    header: { whitespace, node: new Token(header, newTokenId(), RawAst.Token.Type.Ident) },
    body: spaced(body.id),
  }
}

export class Import extends Ast {
  private readonly polyglot_: MultiSegmentAppSegment | null
  private readonly from_: MultiSegmentAppSegment | null
  private readonly import__: MultiSegmentAppSegment
  private readonly all_: NodeChild<Token> | null
  private readonly as_: MultiSegmentAppSegment | null
  private readonly hiding_: MultiSegmentAppSegment | null

  get polyglot(): Ast | null {
    return this.polyglot_?.body ? this.module.get(this.polyglot_.body.node) : null
  }

  get from(): Ast | null {
    return this.from_?.body ? this.module.get(this.from_.body.node) : null
  }

  get import_(): Ast | null {
    return this.import__?.body ? this.module.get(this.import__.body.node) : null
  }

  get all(): Token | null {
    return this.all_?.node ?? null
  }

  get as(): Ast | null {
    return this.as_?.body ? this.module.get(this.as_.body.node) : null
  }

  get hiding(): Ast | null {
    return this.hiding_?.body ? this.module.get(this.hiding_.body.node) : null
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    polyglot: MultiSegmentAppSegment | null,
    from: MultiSegmentAppSegment | null,
    import_: MultiSegmentAppSegment,
    all: NodeChild<Token> | null,
    as: MultiSegmentAppSegment | null,
    hiding: MultiSegmentAppSegment | null,
  ) {
    super(module, id, RawAst.Tree.Type.Import)
    this.polyglot_ = polyglot
    this.from_ = from
    this.import__ = import_
    this.all_ = all
    this.as_ = as
    this.hiding_ = hiding
    setParent(module, this.id, ...this.concreteChildren())
  }

  static Qualified(path: string[], module?: MutableModule): Owned<Import> | undefined {
    const module_ = module ?? MutableModule.Transient()
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const import_ = multiSegmentAppSegment('', 'import', path_)
    return asOwned(new Import(module_, undefined, null, null, import_, null, null, null))
  }

  static Unqualified(
    path: string[],
    name: string,
    module?: MutableModule,
  ): Owned<Import> | undefined {
    const module_ = module ?? MutableModule.Transient()
    const path_ = PropertyAccess.Sequence(path, module)
    if (!path_) return
    const name_ = Ident.new(module_, name)
    const from = multiSegmentAppSegment('', 'from', path_)
    const import_ = multiSegmentAppSegment(' ', 'import', name_)
    return asOwned(new Import(module_, undefined, null, from, import_, null, null, null))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const segment = (segment: MultiSegmentAppSegment | null) => {
      const parts = []
      if (segment) parts.push(segment.header)
      if (segment?.body) parts.push(segment.body)
      return parts
    }
    yield* segment(this.polyglot_)
    yield* segment(this.from_)
    yield* segment(this.import__)
    if (this.all_) yield this.all_
    yield* segment(this.as_)
    yield* segment(this.hiding_)
  }
}

export class TextLiteral extends Ast {
  private readonly open_: NodeChild<Token> | null
  private readonly newline_: NodeChild<Token> | null
  private readonly elements_: NodeChild[]
  private readonly close_: NodeChild<Token> | null

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    open: NodeChild<Token> | null,
    newline: NodeChild<Token> | null,
    elements: NodeChild[],
    close: NodeChild<Token> | null,
  ) {
    super(module, id, RawAst.Tree.Type.TextLiteral)
    setParent(module, this.id, ...elements)
    this.open_ = open
    this.newline_ = newline
    this.elements_ = elements
    this.close_ = close
  }

  static new(rawText: string, moduleIn?: MutableModule) {
    const module = moduleIn ?? MutableModule.Transient()
    const open = unspaced(Token.new("'"))
    const elements = [unspaced(Token.new(escape(rawText)))]
    const close = unspaced(Token.new("'"))
    return asOwned(new TextLiteral(module, undefined, open, null, elements, close))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this.open_) yield this.open_
    if (this.newline_) yield this.newline_
    yield* this.elements_
    if (this.close_) yield this.close_
  }
}

export class Documented extends Ast {
  private readonly open_: NodeChild<Token> | null
  private readonly elements_: NodeChild[]
  private readonly newlines_: NodeChild<Token>[]
  private readonly expression_: NodeChild<AstId> | null

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    open: NodeChild<Token> | null,
    elements: NodeChild[],
    newlines: NodeChild<Token>[],
    expression: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.Documented)
    setParent(module, this.id, expression, ...elements)
    this.open_ = open
    this.elements_ = elements
    this.newlines_ = newlines
    this.expression_ = expression
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this.open_) yield this.open_
    yield* this.elements_
    yield* this.newlines_
    if (this.expression_) yield this.expression_
  }
}

export class Invalid extends Ast {
  private readonly expression_: NodeChild<AstId>

  constructor(module: MutableModule, id: AstId | undefined, expression: NodeChild<AstId>) {
    super(module, id, RawAst.Tree.Type.Invalid)
    setParent(module, this.id, expression)
    this.expression_ = expression
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.expression_
  }
}

export class Group extends Ast {
  private readonly open_: NodeChild<Token> | undefined
  private readonly expression_: NodeChild<AstId> | null
  private readonly close_: NodeChild<Token> | undefined

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<AstId> | null,
    close: NodeChild<Token> | undefined,
  ) {
    super(module, id, RawAst.Tree.Type.Group)
    setParent(module, this.id, expression)
    this.open_ = open
    this.expression_ = expression
    this.close_ = close
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this.open_) yield this.open_
    if (this.expression_) yield this.expression_
    if (this.close_) yield this.close_
  }
}

export class NumericLiteral extends Ast {
  private readonly tokens_: NodeChild[]

  constructor(module: MutableModule, id: AstId | undefined, tokens: NodeChild[]) {
    super(module, id, RawAst.Tree.Type.Number)
    setParent(module, this.id, ...tokens)
    this.tokens_ = tokens ?? []
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this.tokens_.values()
  }
}

type FunctionArgument = NodeChild[]

export class Function extends Ast {
  private readonly name_: NodeChild<AstId>
  private readonly args_: FunctionArgument[]
  private readonly equals_: NodeChild<Token>
  private readonly body_: NodeChild<AstId> | null

  /** @internal */
  static new(
    module: MutableModule,
    name: Owned,
    args: FunctionArgument[],
    body: Owned | null,
  ): Owned<Function> {
    const id = newAstId()
    return asOwned(
      new Function(
        module,
        id,
        unspaced(makeChild(module, name, id)),
        args.map((arg) =>
          arg.map((child) => ({
            ...child,
            node:
              child.node instanceof Token
                ? child.node
                : makeChild(module, module.take(child.node)!.node, id),
          })),
        ),
        undefined,
        body != null ? unspaced(makeChild(module, body, id)) : null,
      ),
    )
  }

  static fromExprs(
    module: MutableModule,
    name: string,
    args: Owned[],
    exprs: Owned[],
    trailingNewline?: boolean,
  ): Owned<Function> {
    const id = newAstId()
    const exprs_: OwnedBlockLine[] = exprs.map((expr) => ({ expression: { node: expr } }))
    if (trailingNewline) {
      exprs_.push({ newline: { node: Token.new('\n') }, expression: null })
    }
    const body = BodyBlock.new(exprs_, module)
    const args_ = args.map((arg) => [{ node: makeChild(module, arg, id) }])
    const ident = { node: Ident.new(module, name).id }
    const equals = { node: Token.new('=') }
    return asOwned(new Function(module, id, ident, args_, equals, { node: body.id }))
  }

  // FIXME for #8367: This should not be nullable. If the `ExprId` has been deleted, the same placeholder logic should be applied
  //  here and in `rawChildren` (and indirectly, `print`).
  get name(): Ast | null {
    return this.module.get(this.name_.node)
  }
  get body(): Ast | null {
    return this.body_ ? this.module.get(this.body_.node) : null
  }

  argNodes(): FunctionArgument[] {
    return [...this.args_]
  }

  *bodyExpressions(): IterableIterator<Ast> {
    const body = this.body_ ? this.module.get(this.body_.node) : null
    if (body instanceof BodyBlock) {
      yield* body.statements()
    } else if (body !== null) {
      yield body
    }
  }

  /** @internal */
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    name: NodeChild<AstId>,
    args: FunctionArgument[],
    equals: NodeChild<Token> | undefined, // Edits (#8367): NodeChild<Tok> | undefined
    body: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.Function)
    this.name_ = name
    this.args_ = args
    this.equals_ = equals ?? spaced(new Token('=', newTokenId(), RawAst.Token.Type.Operator))

    this.body_ = body
    setParent(module, this.id, ...this.concreteChildren())
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.name_
    for (const arg of this.args_) yield* arg
    yield { whitespace: this.equals_.whitespace ?? ' ', node: this.equals_.node }
    if (this.body_ !== null) {
      yield this.body_
    }
  }
}

export class Assignment extends Ast {
  private readonly pattern_: NodeChild<AstId>
  private readonly equals_: NodeChild<Token>
  private readonly expression_: NodeChild<AstId>
  get pattern(): Ast | null {
    return this.module.get(this.pattern_.node)
  }
  get expression(): Ast | null {
    return this.module.get(this.expression_.node)
  }
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    pattern: NodeChild<AstId>,
    equals: NodeChild<Token> | undefined,
    expression: NodeChild<AstId>,
  ) {
    super(module, id, RawAst.Tree.Type.Assignment)
    setParent(module, this.id, pattern, expression)
    this.pattern_ = pattern
    this.equals_ =
      equals ??
      spacedIf(new Token('=', newTokenId(), RawAst.Token.Type.Operator), !!expression.whitespace)
    this.expression_ = expression
  }

  static new(module: MutableModule, ident: string, expression: Owned): Owned<Assignment> {
    const id = newAstId()
    const pattern = unspaced(makeChild(module, Ident.new(module, ident), id))
    const expression_ = spaced(makeChild(module, expression, id))
    return asOwned(new Assignment(module, id, pattern, undefined, expression_))
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.pattern_
    yield {
      whitespace: this.equals_.whitespace ?? this.expression_.whitespace ?? ' ',
      node: this.equals_.node,
    }
    if (this.expression_ !== null) {
      yield this.expression_
    }
  }
}

export class BodyBlock extends Ast {
  private readonly lines_: RawBlockLine[]

  static new(lines: OwnedBlockLine[], module?: MutableModule) {
    const module_ = module ?? MutableModule.Transient()
    const id = newAstId()
    const rawLines = lines.map((line) => lineToRaw(line, module_, id))
    return asOwned(new BodyBlock(module_, id, rawLines))
  }

  lines(): BlockLine[] {
    return this.lines_.map((line) => lineFromRaw(line, this.module))
  }

  takeLines(edit: MutableModule): OwnedBlockLine[] {
    return this.lines_.map((line) => ownedLineFromRaw(line, edit))
  }

  *statements(): IterableIterator<Ast> {
    for (const line of this.lines()) {
      if (line.expression) yield line.expression.node
    }
  }

  constructor(module: MutableModule, id: AstId | undefined, lines: RawBlockLine[]) {
    super(module, id, RawAst.Tree.Type.BodyBlock)
    this.lines_ = lines
    setParent(module, this.id, ...this.concreteChildren())
  }

  push(module: MutableModule, node: Owned) {
    const line = { expression: autospaced(makeChild(module, node, this.id)) }
    const edited = new BodyBlock(module, this.id, [...this.lines_, line])
    edited.parent = this.parent
  }

  /** Insert the given expression(s) starting at the specified line index. */
  insert(module: MutableModule, index: number, ...nodes: Owned[]) {
    const before = this.lines_.slice(0, index)
    const insertions = Array.from(nodes, (node) => ({
      expression: unspaced(makeChild(module, node, this.id)),
    }))
    const after = this.lines_.slice(index)
    const edited = new BodyBlock(module, this.id, [...before, ...insertions, ...after])
    edited.parent = this.parent
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    for (const line of this.lines_) {
      yield line.newline ?? { node: new Token('\n', newTokenId(), RawAst.Token.Type.Newline) }
      if (line.expression !== null) yield line.expression
    }
  }

  printSubtree(
    info: InfoMap,
    offset: number,
    parentIndent: string | null,
    moduleOverride?: Module | undefined,
  ): string {
    const module_ = moduleOverride ?? this.module
    let blockIndent: string | undefined
    let code = ''
    for (const line of this.lines_) {
      // Skip deleted lines (and associated whitespace).
      if (line.expression?.node != null && module_.get(line.expression.node) === null) continue
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
        const lineNode = module_.get(line.expression.node)!
        assertEqual(lineNode.id, line.expression.node)
        assertEqual(lineNode.parent, this.id)
        code += lineNode.printSubtree(info, offset + code.length, blockIndent, moduleOverride)
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this.id])
    } else {
      infos.unshift(this.id)
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
type OwnedBlockLine = Line<Owned>

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

function ownedLineFromRaw(raw: RawBlockLine, module: MutableModule): OwnedBlockLine {
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

function lineToRaw(line: OwnedBlockLine, module: MutableModule, block: AstId): RawBlockLine {
  return {
    newline: line.newline,
    expression: line.expression
      ? {
          whitespace: line.expression?.whitespace,
          node: makeChild(module, line.expression.node, block),
        }
      : null,
  }
}

export class Ident extends Ast {
  private readonly token: NodeChild<Token>

  constructor(module: MutableModule, id: AstId | undefined, token: NodeChild<Token>) {
    super(module, id, RawAst.Tree.Type.Ident)
    this.token = token
  }

  static new(module: MutableModule, code: string) {
    return asOwned(
      new Ident(
        module,
        undefined,
        unspaced(new Token(code, newTokenId(), RawAst.Token.Type.Ident)),
      ),
    )
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

export class Wildcard extends Ast {
  private readonly token: NodeChild<Token>

  constructor(module: MutableModule, id: AstId | undefined, token: NodeChild<Token>) {
    super(module, id, RawAst.Tree.Type.Wildcard)
    this.token = token
  }

  static new(module?: MutableModule) {
    const module_ = module ?? MutableModule.Transient()
    return asOwned(
      new Wildcard(
        module_,
        undefined,
        unspaced(new Token('_', newTokenId(), RawAst.Token.Type.Wildcard)),
      ),
    )
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  info: InfoMap | undefined,
): { whitespace: string | undefined; node: AstId } {
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
): { whitespace: string | undefined; node: AstId } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesExpected, tokens, tokensOut)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokens, tokensOut)
  const visitChildren = (tree: LazyObject) => {
    const children: NodeChild[] = []
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
        let expression = null
        if (line.expression != null) {
          expression = recurseTree(line.expression)
        }
        return { newline, expression }
      })
      const id = nodesExpected.get(spanKey)?.pop()
      node = new BodyBlock(module, id, lines).id
      break
    }
    case RawAst.Tree.Type.Function: {
      const name = recurseTree(tree.name)
      const args = Array.from(tree.args, (arg) => visitChildren(arg))
      const equals = recurseToken(tree.equals)
      const body = tree.body !== undefined ? recurseTree(tree.body) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Function(module, id, name, args, equals, body).id
      break
    }
    case RawAst.Tree.Type.Ident: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Ident(module, id, recurseToken(tree.token)).id
      break
    }
    case RawAst.Tree.Type.Assignment: {
      const pattern = recurseTree(tree.pattern)
      const equals = recurseToken(tree.equals)
      const value = recurseTree(tree.expr)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Assignment(module, id, pattern, equals, value).id
      break
    }
    case RawAst.Tree.Type.App: {
      const func = recurseTree(tree.func)
      const arg = recurseTree(tree.arg)
      const id = nodesExpected.get(spanKey)?.pop()
      node = positionalApp(module, id, func, arg).id
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
      node = namedApp(module, id, func, leftParen, name, equals, arg, rightParen).id
      break
    }
    case RawAst.Tree.Type.UnaryOprApp: {
      const opr = recurseToken(tree.opr)
      const arg = tree.rhs ? recurseTree(tree.rhs) : null
      const id = nodesExpected.get(spanKey)?.pop()
      if (opr.node.code() === '-') {
        node = new NegationOprApp(module, id, opr, arg).id
      } else {
        node = new UnaryOprApp(module, id, opr, arg).id
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
        node = new PropertyAccess(module, id, lhs, token, rhs).id
      } else {
        node = new OprApp(module, id, lhs, opr, rhs).id
      }
      break
    }
    case RawAst.Tree.Type.Number: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new NumericLiteral(module, id, visitChildren(tree)).id
      break
    }
    case RawAst.Tree.Type.Wildcard: {
      const token = recurseToken(tree.token)
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Wildcard(module, id, token).id
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
      node = new Invalid(module, id, expression).id
      break
    }
    case RawAst.Tree.Type.Group: {
      const open = tree.open ? recurseToken(tree.open) : undefined
      const expression = tree.body ? recurseTree(tree.body) : null
      const close = tree.close ? recurseToken(tree.close) : undefined
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Group(module, id, open, expression, close).id
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
      node = new TextLiteral(module, id, open, newline, elements, close).id
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
      node = new Documented(module, id, open, elements, newlines, expression).id
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
      node = new Import(module, id, polyglot, from, import_, all, as, hiding).id
      break
    }
    default: {
      const id = nodesExpected.get(spanKey)?.pop()
      node = new Generic(module, id, visitChildren(tree), tree.type).id
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
  const id = tokens.get(key) ?? newTokenId()
  const node = new Token(tokenCode, id, token.type)
  tokensOut.set(key, id)
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
    root.id,
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

export function parseTransitional(code: string, idMap: IdMap): Owned<BodyBlock> {
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
      tokens.set(tokenKey(start, length), token.astId as Uuid as TokenId)
    } else if (nodeOrToken.isTree()) {
      const node: RawAstExtended<RawAst.Tree> = nodeOrToken
      if (
        node.isTree(RawAst.Tree.Type.OprSectionBoundary) ||
        node.isTree(RawAst.Tree.Type.TemplateFunction)
      )
        return true
      let id = node.astId as Uuid as AstId
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
  newRoot.module.raw.astExtended = astExtended
  newRoot.module.raw.spans = spans
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
  const printed = print(newRoot.id, newRoot.module)
  for (const [key, ids] of printed.info.nodes) {
    const range = keyToRange(key)
    idMap.insertKnownId([range.start, range.end], ids[0]! as Uuid as ExternalId)
  }
  for (const [key, id] of printed.info.tokens) {
    const range = keyToRange(key)
    idMap.insertKnownId([range.start, range.end], id as Uuid as ExternalId)
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
