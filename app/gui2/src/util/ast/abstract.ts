import * as RawAst from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import type { Opt } from '@/util/data/opt'
import { Err, Ok, type Result } from '@/util/data/result'
import type { LazyObject } from '@/util/parserSupport'
import { unsafeEntries } from '@/util/record'
import * as random from 'lib0/random'
import { IdMap, type ExprId } from 'shared/yjsModel'
import { reactive } from 'vue'

export interface Module {
  get raw(): MutableModule
  get(id: AstId): Ast | null
  getExtended(id: AstId): RawAstExtended | undefined
  edit(): MutableModule
  apply(module: MutableModule): void
}

export class MutableModule implements Module {
  base: Module | null
  nodes: Map<AstId, Ast | null>
  astExtended: Map<AstId, RawAstExtended> | null

  constructor(
    base: Module | null,
    nodes: Map<AstId, Ast | null>,
    astExtended: Map<AstId, RawAstExtended> | null,
  ) {
    this.base = base
    this.nodes = nodes
    this.astExtended = astExtended
  }

  static Observable(): MutableModule {
    const nodes = reactive(new Map<AstId, Ast>())
    const astExtended = reactive(new Map<AstId, RawAstExtended>())
    return new MutableModule(null, nodes, astExtended)
  }

  static Transient(): MutableModule {
    const nodes = new Map<AstId, Ast>()
    return new MutableModule(null, nodes, null)
  }

  edit(): MutableModule {
    const nodes = new Map<AstId, Ast>()
    return new MutableModule(this, nodes, null)
  }

  get raw(): MutableModule {
    return this
  }

  apply(edit: MutableModule) {
    for (const [id, ast] of edit.nodes.entries()) {
      if (ast === null) {
        this.nodes.delete(id)
      } else {
        this.nodes.set(id, ast)
      }
    }
    if (edit.astExtended) {
      if (this.astExtended)
        console.error(`Merging astExtended not implemented, probably doesn't make sense`)
      this.astExtended = edit.astExtended
    }
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

  set(id: AstId, ast: Ast) {
    this.nodes.set(id, ast)
  }

  getExtended(id: AstId): RawAstExtended | undefined {
    return this.astExtended?.get(id) ?? this.base?.getExtended(id)
  }

  delete(id: AstId) {
    this.nodes.set(id, null)
  }
}

// TODO (#8367): Flatten overlay into input `committed`.
/*
export function normalize(rootIn: Ast): Ast {
  const printed = print(rootIn)
  const module = new Module()
  const tree = parseEnso(printed.code)
  const rootOut = abstract(module, tree, printed.code, printed.info).node
  module.syncCommittedFromEdited()
  return module.get(rootOut)!
}
 */

export type NodeChild<T = AstId | Token> = { whitespace?: string | undefined; node: T }

declare const brandAstId: unique symbol
declare const brandTokenId: unique symbol
export type AstId = ExprId & { [brandAstId]: never }
export type TokenId = ExprId & { [brandTokenId]: never }

function newNodeId(): AstId {
  return random.uuidv4() as AstId
}
function newTokenId(): TokenId {
  return random.uuidv4() as TokenId
}

export class Token {
  code_: string
  exprId: TokenId
  tokenType_: RawAst.Token.Type | undefined
  constructor(code: string, id: TokenId, type: RawAst.Token.Type | undefined) {
    this.code_ = code
    this.exprId = id
    this.tokenType_ = type
  }

  static new(code: string) {
    return new Token(code, newTokenId(), undefined)
  }

  // Compatibility wrapper for `exprId`.
  get astId(): TokenId {
    return this.exprId
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
  _id: AstId
  readonly module: Module

  // Deprecated interface for incremental integration of Ast API. Eliminate usages for #8367.
  get astExtended(): RawAstExtended | undefined {
    return this.module.getExtended(this._id)
  }

  serialize(): string {
    return JSON.stringify(print(this))
  }

  static deserialize(serialized: string): Ast {
    const parsed: SerializedPrintedSource = JSON.parse(serialized)
    const nodes = new Map(unsafeEntries(parsed.info.nodes))
    const tokens = new Map(unsafeEntries(parsed.info.tokens))
    const module = MutableModule.Transient()
    const tree = parseEnso(parsed.code)
    const root = abstract(module, tree, parsed.code, { nodes, tokens }).node
    return module.get(root)!
  }

  get exprId(): AstId {
    return this._id
  }

  get astId(): AstId {
    return this._id
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

  /** Returns child subtrees, including information about the whitespace between them. */
  abstract concreteChildren(): IterableIterator<NodeChild>

  code(module?: Module): string {
    return print(this, module).code
  }

  repr(): string {
    return this.code()
  }

  typeName(): string | undefined {
    if (this.treeType === undefined) return undefined
    return RawAst.Tree.typeNames[this.treeType]
  }

  static parse(source: PrintedSource | string, inModule?: MutableModule): Ast {
    const code = typeof source === 'object' ? source.code : source
    const ids = typeof source === 'object' ? source.info : undefined
    const tree = parseEnso(code)
    const module = inModule ?? MutableModule.Observable()
    const newRoot = abstract(module, tree, code, ids).node
    return module.get(newRoot)!
  }

  static parseLine(source: PrintedSource | string): Ast {
    const ast = Ast.parse(source)
    if (ast instanceof BodyBlock) {
      const [expr] = ast.expressions()
      return expr instanceof Ast ? expr : ast
    } else {
      return ast
    }
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
    this._id = id ?? newNodeId()
    this.treeType = treeType
    module.set(this._id, this)
  }

  _print(
    info: InfoMap,
    offset: number,
    indent: string,
    moduleOverride?: Module | undefined,
  ): string {
    const module_ = moduleOverride ?? this.module
    let code = ''
    for (const child of this.concreteChildren()) {
      if (child.node != null && !(child.node instanceof Token) && module_.get(child.node) === null)
        continue
      if (child.whitespace != null) {
        code += child.whitespace
      } else if (code.length != 0) {
        // TODO for #8367: Identify cases where a space should not be inserted.
        code += ' '
      }
      if (child.node != null) {
        if (child.node instanceof Token) {
          code += child.node.code()
        } else {
          code += module_
            .get(child.node)!
            ._print(info, offset + code.length, indent, moduleOverride)
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this._id])
    } else {
      infos.push(this._id)
    }
    return code
  }
}

export class App extends Ast {
  _func: NodeChild<AstId>
  _leftParen: NodeChild<Token> | null
  _argumentName: NodeChild<Token> | null
  _equals: NodeChild<Token> | null
  _arg: NodeChild<AstId>
  _rightParen: NodeChild<Token> | null

  get function(): Ast {
    return this.module.get(this._func.node)!
  }

  get argumentName(): Token | null {
    return this._argumentName?.node ?? null
  }

  get argument(): Ast {
    return this.module.get(this._arg.node)!
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
    this._func = func
    this._leftParen = leftParen
    this._argumentName = name
    this._equals = equals
    this._arg = arg
    this._rightParen = rightParen
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._func
    if (this._leftParen) yield this._leftParen
    if (this._argumentName) yield this._argumentName
    if (this._equals)
      yield { whitespace: this._equals.whitespace ?? this._arg.whitespace, node: this._equals.node }
    yield this._arg
    if (this._rightParen) yield this._rightParen
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
  return new App(
    module,
    id,
    func,
    leftParen,
    name,
    equals,
    arg,
    rightParen,
    RawAst.Tree.Type.NamedApp,
  )
}

export class UnaryOprApp extends Ast {
  _opr: NodeChild<Token>
  _arg: NodeChild<AstId> | null

  get operator(): Token {
    return this._opr.node
  }

  get argument(): Ast | null {
    const id = this._arg?.node
    return id ? this.module.get(id) : null
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    opr: NodeChild<Token>,
    arg: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.UnaryOprApp)
    this._opr = opr
    this._arg = arg
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._opr
    if (this._arg) yield this._arg
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
  _lhs: NodeChild<AstId> | null
  _opr: NodeChild[]
  _rhs: NodeChild<AstId> | null

  get lhs(): Ast | null {
    return this._lhs ? this.module.get(this._lhs.node) : null
  }

  get operator(): Result<Token, NodeChild[]> {
    const first = this._opr[0]?.node
    if (first && this._opr.length < 2 && first instanceof Token) {
      return Ok(first)
    } else {
      return Err(this._opr)
    }
  }

  get rhs(): Ast | null {
    return this._rhs ? this.module.get(this._rhs.node) : null
  }

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild[],
    rhs: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.OprApp)
    this._lhs = lhs
    this._opr = opr
    this._rhs = rhs
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this._lhs) yield this._lhs
    for (const opr of this._opr) yield opr
    if (this._rhs) yield this._rhs
  }
}

export class PropertyAccess extends OprApp {
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    lhs: NodeChild<AstId> | null,
    opr: NodeChild<Token>,
    rhs: NodeChild<AstId> | null,
  ) {
    super(module, id, lhs, [opr], rhs)
  }
}

/** Representation without any type-specific accessors, for tree types that don't require any special treatment. */
export class Generic extends Ast {
  _children: NodeChild[]

  constructor(
    module: MutableModule,
    id?: AstId,
    children?: NodeChild[],
    treeType?: RawAst.Tree.Type,
  ) {
    super(module, id, treeType)
    this._children = children ?? []
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this._children.values()
  }
}

type MultiSegmentAppSegment = { header: NodeChild<Token>; body: NodeChild<AstId> | null }

export class Import extends Ast {
  _polyglot: MultiSegmentAppSegment | null
  _from: MultiSegmentAppSegment | null
  _import: MultiSegmentAppSegment
  _all: NodeChild<Token> | null
  _as: MultiSegmentAppSegment | null
  _hiding: MultiSegmentAppSegment | null

  get polyglot(): Ast | null {
    return this._polyglot?.body ? this.module.get(this._polyglot.body.node) : null
  }

  get from(): Ast | null {
    return this._from?.body ? this.module.get(this._from.body.node) : null
  }

  get import_(): Ast | null {
    return this._import?.body ? this.module.get(this._import.body.node) : null
  }

  get all(): Token | null {
    return this._all?.node ?? null
  }

  get as(): Ast | null {
    return this._as?.body ? this.module.get(this._as.body.node) : null
  }

  get hiding(): Ast | null {
    return this._hiding?.body ? this.module.get(this._hiding.body.node) : null
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
    this._polyglot = polyglot
    this._from = from
    this._import = import_
    this._all = all
    this._as = as
    this._hiding = hiding
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    const segment = (segment: MultiSegmentAppSegment | null) => {
      const parts = []
      if (segment) parts.push(segment.header)
      if (segment?.body) parts.push(segment.body)
      return parts
    }
    yield* segment(this._polyglot)
    yield* segment(this._from)
    yield* segment(this._import)
    if (this._all) yield this._all
    yield* segment(this._as)
    yield* segment(this._hiding)
  }
}

export class TextLiteral extends Ast {
  _open: NodeChild<Token> | null
  _newline: NodeChild<Token> | null
  _elements: NodeChild[]
  _close: NodeChild<Token> | null

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    open: NodeChild<Token> | null,
    newline: NodeChild<Token> | null,
    elements: NodeChild[],
    close: NodeChild<Token> | null,
  ) {
    super(module, id, RawAst.Tree.Type.TextLiteral)
    this._open = open
    this._newline = newline
    this._elements = elements
    this._close = close
  }

  static new(rawText: string): TextLiteral {
    const module = MutableModule.Transient()
    const text = Token.new(escape(rawText))
    return new TextLiteral(module, undefined, { node: Token.new("'") }, null, [{ node: text }], {
      node: Token.new("'"),
    })
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this._open) yield this._open
    if (this._newline) yield this._newline
    yield* this._elements
    if (this._close) yield this._close
  }
}

export class Invalid extends Ast {
  _expression: NodeChild<AstId>

  constructor(module: MutableModule, id: AstId | undefined, expression: NodeChild<AstId>) {
    super(module, id, RawAst.Tree.Type.Invalid)
    this._expression = expression
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._expression
  }
}

export class Group extends Ast {
  _open: NodeChild<Token> | undefined
  _expression: NodeChild<AstId> | null
  _close: NodeChild<Token> | undefined

  constructor(
    module: MutableModule,
    id: AstId | undefined,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<AstId> | null,
    close: NodeChild<Token> | undefined,
  ) {
    super(module, id, RawAst.Tree.Type.Group)
    this._open = open
    this._expression = expression
    this._close = close
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this._open) yield this._open
    if (this._expression) yield this._expression
    if (this._close) yield this._close
  }
}

export class NumericLiteral extends Ast {
  _tokens: NodeChild[]

  constructor(module: MutableModule, id: AstId | undefined, tokens: NodeChild[]) {
    super(module, id, RawAst.Tree.Type.Number)
    this._tokens = tokens ?? []
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this._tokens.values()
  }
}

type FunctionArgument = NodeChild[]

export class Function extends Ast {
  _name: NodeChild<AstId>
  _args: FunctionArgument[]
  _equals: NodeChild<Token>
  _body: NodeChild<AstId> | null
  // FIXME for #8367: This should not be nullable. If the `ExprId` has been deleted, the same placeholder logic should be applied
  //  here and in `rawChildren` (and indirectly, `print`).
  get name(): Ast | null {
    return this.module.get(this._name.node)
  }
  get body(): Ast | null {
    return this._body ? this.module.get(this._body.node) : null
  }
  *bodyExpressions(): IterableIterator<Ast> {
    const body = this._body ? this.module.get(this._body.node) : null
    if (body instanceof BodyBlock) {
      yield* body.expressions()
    } else if (body !== null) {
      yield body
    }
  }
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    name: NodeChild<AstId>,
    args: FunctionArgument[],
    equals: NodeChild<Token>, // Edits (#8367): NodeChild<Tok> | undefined
    body: NodeChild<AstId> | null,
  ) {
    super(module, id, RawAst.Tree.Type.Function)
    this._name = name
    this._args = args
    this._equals = equals
    this._body = body
  }
  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._name
    for (const arg of this._args) yield* arg
    yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    if (this._body !== null) {
      yield this._body
    }
  }
}

export class Assignment extends Ast {
  _pattern: NodeChild<AstId>
  _equals: NodeChild<Token>
  _expression: NodeChild<AstId>
  get pattern(): Ast | null {
    return this.module.get(this._pattern.node)
  }
  get expression(): Ast | null {
    return this.module.get(this._expression.node)
  }
  constructor(
    module: MutableModule,
    id: AstId | undefined,
    pattern: NodeChild<AstId>,
    equals: NodeChild<Token>, // TODO: Edits (#8367): Allow undefined
    expression: NodeChild<AstId>,
  ) {
    super(module, id, RawAst.Tree.Type.Assignment)
    this._pattern = pattern
    this._equals = equals
    this._expression = expression
  }
  // TODO: Edits (#8367)
  /*
  static new(
    id: AstId | undefined,
    ident: string,
    equals: WithWhitespace<Tok> | undefined,
    expression: WithWhitespace<AstId>,
  ): Assignment {
    const pattern = { node: Ident.new(ident)._id }
    return new Assignment(id, pattern, equals, expression)
  }
   */
  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._pattern
    yield { whitespace: this._equals.whitespace ?? ' ', node: this._equals.node }
    if (this._expression !== null) {
      yield this._expression
    }
  }
}

interface BlockLine {
  newline: NodeChild<Token> // Edits (#8367): Allow undefined
  expression: NodeChild<AstId> | null
}

export class BodyBlock extends Ast {
  _lines: BlockLine[];

  *expressions(): IterableIterator<Ast> {
    for (const line of this._lines) {
      if (line.expression) {
        const node = this.module.get(line.expression.node)
        if (node) {
          yield node
        } else {
          console.warn(`Missing node:`, line.expression.node)
        }
      }
    }
  }

  constructor(module: MutableModule, id: AstId | undefined, lines: BlockLine[]) {
    super(module, id, RawAst.Tree.Type.BodyBlock)
    this._lines = lines
  }

  // TODO: Edits (#8367)
  /*
  static new(id: AstId | undefined, expressions: Ast[]): Block {
    return new Block(
      id,
      expressions.map((e) => ({ expression: { node: e._id } })),
    )
  }

  pushExpression(node: Ast) {
    if (!edited.has(this._id)) edited.set(this._id, new Block(this._id, [...this._lines]))
    const cow = edited.get(this._id) as Block
    cow._lines.push({ expression: { node: node._id } })
  }
   */

  *concreteChildren(): IterableIterator<NodeChild> {
    for (const line of this._lines) {
      yield line.newline
      if (line.expression !== null) yield line.expression
    }
  }

  _print(
    info: InfoMap,
    offset: number,
    indent: string,
    moduleOverride?: Module | undefined,
  ): string {
    const module_ = moduleOverride ?? this.module
    let code = ''
    for (const line of this._lines) {
      if (line.expression?.node != null && module_.get(line.expression.node) === null) continue
      code += line.newline?.whitespace ?? ''
      code += line.newline?.node.code() ?? '\n'
      if (line.expression !== null) {
        code += line.expression.whitespace ?? indent
        if (line.expression.node !== null) {
          code += module_
            .get(line.expression.node)!
            ._print(info, offset, indent + '    ', moduleOverride)
        }
      }
    }
    const span = nodeKey(offset, code.length, this.treeType)
    const infos = info.nodes.get(span)
    if (infos == null) {
      info.nodes.set(span, [this._id])
    } else {
      infos.push(this._id)
    }
    return code
  }
}

export class Ident extends Ast {
  public token: NodeChild<Token>

  constructor(module: MutableModule, id: AstId | undefined, token: NodeChild<Token>) {
    super(module, id, RawAst.Tree.Type.Ident)
    this.token = token
  }

  // TODO: Edits (#8367)
  /*
  static new(id: AstId | undefined, code: string): Ident {
    return Ident.fromToken(id, { node: token(code, RawAst.Token.Type.Ident) })
  }
   */

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

export class Wildcard extends Ast {
  public token: NodeChild<Token>

  constructor(module: MutableModule, id: AstId | undefined, token: NodeChild<Token>) {
    super(module, id, RawAst.Tree.Type.Wildcard)
    this.token = token
  }

  static new(): Wildcard {
    const module = MutableModule.Transient()
    const ast = new Wildcard(module, undefined, {
      node: new Token('_', newTokenId(), RawAst.Token.Type.Wildcard),
    })
    return ast
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

export class RawCode extends Ast {
  _code: NodeChild

  constructor(module: MutableModule, id: AstId | undefined, code: NodeChild) {
    super(module, id)
    this._code = code
  }

  // TODO (#8367)
  /*
  static new(id: AstId | undefined, code: string): RawCode {
    const token = new Token(code, newTokenId(), RawAst.Token.Type.Ident)
    return new RawCode(new Module(), id, { node: token })
  }
   */

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this._code
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
  const tokenIds = info?.tokens ?? new Map()
  return abstractTree(module, tree, code, nodesExpected, tokenIds)
}

function abstractTree(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  nodesExpected: NodeSpanMap,
  tokenIds: TokenSpanMap,
): { whitespace: string | undefined; node: AstId } {
  const recurseTree = (tree: RawAst.Tree) =>
    abstractTree(module, tree, code, nodesExpected, tokenIds)
  const recurseToken = (token: RawAst.Token.Token) => abstractToken(token, code, tokenIds)
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
      visitChildren(tree)
      const close = tree.close ? recurseToken(tree.close) : null
      const id = nodesExpected.get(spanKey)?.pop()
      node = new TextLiteral(module, id, open, newline, elements, close).exprId
      break
    }
    case RawAst.Tree.Type.Import: {
      const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
        header: recurseToken(segment.header),
        body: segment.body ? recurseTree(segment.body) : null,
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
  tokenIds: TokenSpanMap,
): { whitespace: string; node: Token } {
  const whitespaceStart = token.whitespaceStartInCodeBuffer
  const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
  const whitespace = code.substring(whitespaceStart, whitespaceEnd)
  const codeStart = token.startInCodeBuffer
  const codeEnd = codeStart + token.lengthInCodeBuffer
  const tokenCode = code.substring(codeStart, codeEnd)
  const key = tokenKey(codeStart, codeEnd - codeStart)
  const exprId = tokenIds.get(key) ?? newTokenId()
  const node = new Token(tokenCode, exprId, token.type)
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
}

interface PrintedSource {
  info: InfoMap
  code: string
}

/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast, module?: Module | undefined): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = ast._print(info, 0, '', module)
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

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(module: Module, name: string): Function | null {
  for (const node of module.raw.nodes.values()) {
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

/*
export function insertNewNodeAST(
  block: BodyBlock,
  ident: string,
  expression: string,
): { assignment: AstId; value: AstId } {
  const value = RawCode.new(undefined, expression)._id
  const assignment = Assignment.new(undefined, ident, undefined, { node: value })
  block.pushExpression(assignment)
  return { assignment: assignment._id, value }
}

export function deleteExpressionAST(ast: Ast) {
  ast.delete()
}

export function replaceExpressionContentAST(id: AstId, code: string) {
  return RawCode.new(id, code)
}
 */

/** Parse using new AST types with old IdMap synchronization/editing. */
export function parseTransitional(code: string, idMap: IdMap): Ast {
  const legacyAst = RawAstExtended.parse(code, idMap)
  idMap.finishAndSynchronize()
  const nodes = new Map<NodeKey, AstId[]>()
  const tokens = new Map<TokenKey, TokenId>()
  const astExtended = reactive(new Map<AstId, RawAstExtended>())
  legacyAst.visitRecursive((nodeOrToken: RawAstExtended<RawAst.Tree | RawAst.Token>) => {
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
        id = newNodeId()
      }
      astExtended.set(id, nodeOrToken)
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
  const newRoot = Ast.parse({ info: { nodes, tokens }, code })
  newRoot.module.raw.astExtended = astExtended
  return newRoot
}

export const parse = Ast.parse
export const parseLine = Ast.parseLine

export function deserialize(serialized: string): Ast {
  return Ast.deserialize(serialized)
}

declare const AstKey: unique symbol
declare const TokenKey: unique symbol
declare module '@/providers/widgetRegistry' {
  export interface WidgetInputTypes {
    [AstKey]: Ast
    [TokenKey]: Token
  }
}
