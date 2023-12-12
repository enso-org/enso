import * as RawAst from '@/generated/ast'
import { parseEnso } from '@/util/ast'
import { AstExtended as RawAstExtended } from '@/util/ast/extended'
import { Err, Ok, type Result } from '@/util/data/result'
import type { LazyObject } from '@/util/parserSupport'
import * as random from 'lib0/random'
import { IdMap, type ExprId } from 'shared/yjsModel'
import { reactive } from 'vue'

interface Module {
  get(id: AstId): Ast | null
  getExtended(id: AstId): RawAstExtended | undefined
}

class Committed implements Module {
  nodes: Map<AstId, Ast>
  astExtended: Map<AstId, RawAstExtended>

  constructor() {
    this.nodes = reactive(new Map<AstId, Ast>())
    this.astExtended = reactive(new Map<AstId, RawAstExtended>())
  }

  /** Returns a syntax node representing the current committed state of the given ID. */
  get(id: AstId): Ast | null {
    return this.nodes.get(id) ?? null
  }

  getExtended(id: AstId): RawAstExtended | undefined {
    return this.astExtended.get(id)
  }
}

class Edit implements Module {
  base: Committed
  pending: Map<AstId, Ast | null>

  constructor(base: Committed) {
    this.base = base
    this.pending = new Map()
  }

  /** Replace all committed values with the state of the uncommitted parse. */
  commit() {
    for (const [id, ast] of this.pending.entries()) {
      if (ast === null) {
        this.base.nodes.delete(id)
      } else {
        this.base.nodes.set(id, ast)
      }
    }
    this.pending.clear()
  }

  /** Returns a syntax node representing the current committed state of the given ID. */
  get(id: AstId): Ast | null {
    const editedNode = this.pending.get(id)
    if (editedNode === null) {
      return null
    } else {
      return editedNode ?? this.base.get(id) ?? null
    }
  }

  set(id: AstId, ast: Ast) {
    this.pending.set(id, ast)
  }

  getExtended(id: AstId): RawAstExtended | undefined {
    return this.base.astExtended.get(id)
  }

  delete(id: AstId) {
    this.pending.set(id, null)
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
  private _code: string
  exprId: TokenId
  readonly _tokenType: RawAst.Token.Type
  constructor(code: string, id: TokenId, type: RawAst.Token.Type) {
    this._code = code
    this.exprId = id
    this._tokenType = type
  }

  // Compatibility wrapper for `exprId`.
  get astId(): TokenId {
    return this.exprId
  }

  code(): string {
    return this._code
  }

  typeName(): string {
    return RawAst.Token.typeNames[this._tokenType]!
  }
}

export abstract class Ast {
  readonly treeType: RawAst.Tree.Type | undefined
  _id: AstId
  readonly module: Committed

  // Deprecated interface for incremental integration of Ast API. Eliminate usages for #8367.
  get astExtended(): RawAstExtended | undefined {
    return this.module.getExtended(this._id)
  }

  serialize(): string {
    return JSON.stringify(print(this))
  }

  static deserialize(serialized: string): Ast {
    const parsed: any = JSON.parse(serialized)
    const nodes: NodeSpanMap = new Map(Object.entries(parsed.info.nodes))
    const tokens: TokenSpanMap = new Map(Object.entries(parsed.info.tokens))
    const module = new Committed()
    const edit = new Edit(module)
    const tree = parseEnso(parsed.code)
    type NodeSpanMap = Map<NodeKey, AstId[]>
    type TokenSpanMap = Map<TokenKey, TokenId>
    const root = abstract(edit, tree, parsed.code, { nodes, tokens }).node
    edit.commit()
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

  code(): string {
    return print(this).code
  }

  repr(): string {
    return this.code()
  }

  typeName(): string | undefined {
    if (this.treeType === undefined) return undefined
    return RawAst.Tree.typeNames[this.treeType]
  }

  static parse(source: PrintedSource | string): Ast {
    const code = typeof source === 'object' ? source.code : source
    const ids = typeof source === 'object' ? source.info : undefined
    const tree = parseEnso(code)
    const module = new Committed()
    const edit = new Edit(module)
    const newRoot = abstract(edit, tree, code, ids).node
    edit.commit()
    return module.get(newRoot)!
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

  protected constructor(module: Edit, id?: AstId, treeType?: RawAst.Tree.Type) {
    this.module = module.base
    this._id = id ?? newNodeId()
    this.treeType = treeType
    module.set(this._id, this)
  }

  _print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const child of this.concreteChildren()) {
      if (
        child.node != null &&
        !(child.node instanceof Token) &&
        this.module.get(child.node) === null
      )
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
          code += this.module.get(child.node)!._print(info, offset + code.length, indent)
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
  private _func: NodeChild<AstId>
  private _leftParen: NodeChild<Token> | null
  private _argumentName: NodeChild<Token> | null
  private _equals: NodeChild<Token> | null
  private _arg: NodeChild<AstId>
  private _rightParen: NodeChild<Token> | null

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
    module: Edit,
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

function positionalApp(
  module: Edit,
  id: AstId | undefined,
  func: NodeChild<AstId>,
  arg: NodeChild<AstId>,
): App {
  return new App(module, id, func, null, null, null, arg, null, RawAst.Tree.Type.App)
}

function namedApp(
  module: Edit,
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
  private _opr: NodeChild<Token>
  private _arg: NodeChild<AstId> | null

  get operator(): Token {
    return this._opr.node
  }

  get argument(): Ast | null {
    const id = this._arg?.node
    return id ? this.module.get(id) : null
  }

  constructor(
    module: Edit,
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
    module: Edit,
    id: AstId | undefined,
    opr: NodeChild<Token>,
    arg: NodeChild<AstId> | null,
  ) {
    super(module, id, opr, arg)
  }
}

export class OprApp extends Ast {
  protected _lhs: NodeChild<AstId> | null
  protected _opr: NodeChild[]
  protected _rhs: NodeChild<AstId> | null

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
    module: Edit,
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
    module: Edit,
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
  private readonly _children: NodeChild[]

  constructor(module: Edit, id?: AstId, children?: NodeChild[], treeType?: RawAst.Tree.Type) {
    super(module, id, treeType)
    this._children = children ?? []
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this._children.values()
  }
}

type MultiSegmentAppSegment = { header: NodeChild<Token>; body: NodeChild<AstId> | null }

export class Import extends Ast {
  private polyglot_: MultiSegmentAppSegment | null
  private from_: MultiSegmentAppSegment | null
  private import__: MultiSegmentAppSegment
  private all_: NodeChild<Token> | null
  private as_: MultiSegmentAppSegment | null
  private hiding_: MultiSegmentAppSegment | null

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
    module: Edit,
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
    module: Edit,
    id: AstId | undefined,
    open: NodeChild<Token> | null,
    newline: NodeChild<Token> | null,
    elements: NodeChild[],
    close: NodeChild<Token> | null,
  ) {
    super(module, id, RawAst.Tree.Type.TextLiteral)
    this.open_ = open
    this.newline_ = newline
    this.elements_ = elements
    this.close_ = close
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    if (this.open_) yield this.open_
    if (this.newline_) yield this.newline_
    yield* this.elements_
    if (this.close_) yield this.close_
  }
}

export class Invalid extends Ast {
  private readonly expression_: NodeChild<AstId>

  constructor(module: Edit, id: AstId | undefined, expression: NodeChild<AstId>) {
    super(module, id, RawAst.Tree.Type.Invalid)
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
    module: Edit,
    id: AstId | undefined,
    open: NodeChild<Token> | undefined,
    expression: NodeChild<AstId> | null,
    close: NodeChild<Token> | undefined,
  ) {
    super(module, id, RawAst.Tree.Type.Group)
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
  private readonly _tokens: NodeChild[]

  constructor(module: Edit, id: AstId | undefined, tokens: NodeChild[]) {
    super(module, id, RawAst.Tree.Type.Number)
    this._tokens = tokens ?? []
  }

  concreteChildren(): IterableIterator<NodeChild> {
    return this._tokens.values()
  }
}

type FunctionArgument = NodeChild[]
export class Function extends Ast {
  private _name: NodeChild<AstId>
  private _args: FunctionArgument[]
  private _equals: NodeChild<Token>
  private _body: NodeChild<AstId> | null
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
    module: Edit,
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
  private _pattern: NodeChild<AstId>
  private _equals: NodeChild<Token>
  private _expression: NodeChild<AstId>
  get pattern(): Ast | null {
    return this.module.get(this._pattern.node)
  }
  get expression(): Ast | null {
    return this.module.get(this._expression.node)
  }
  constructor(
    module: Edit,
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

type BlockLine = {
  newline: NodeChild<Token> // Edits (#8367): Allow undefined
  expression: NodeChild<AstId> | null
}
export class BodyBlock extends Ast {
  private _lines: BlockLine[];

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

  constructor(module: Edit, id: AstId | undefined, lines: BlockLine[]) {
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

  _print(info: InfoMap, offset: number, indent: string): string {
    let code = ''
    for (const line of this._lines) {
      if (line.expression?.node != null && this.module.get(line.expression.node) === null) continue
      code += line.newline?.whitespace ?? ''
      code += line.newline?.node.code() ?? '\n'
      if (line.expression !== null) {
        code += line.expression.whitespace ?? indent
        if (line.expression.node !== null) {
          code += this.module.get(line.expression.node)!._print(info, offset, indent + '    ')
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

  constructor(module: Edit, id: AstId | undefined, token: NodeChild<Token>) {
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

  constructor(module: Edit, id: AstId | undefined, token: NodeChild<Token>) {
    super(module, id, RawAst.Tree.Type.Wildcard)
    this.token = token
  }

  static new(): Wildcard {
    const module = new Committed()
    const edit = new Edit(module)
    const ast = new Wildcard(edit, undefined, {
      node: new Token('_', newTokenId(), RawAst.Token.Type.Wildcard),
    })
    edit.commit()
    return ast
  }

  *concreteChildren(): IterableIterator<NodeChild> {
    yield this.token
  }
}

export class RawCode extends Ast {
  private _code: NodeChild

  constructor(module: Edit, id: AstId | undefined, code: NodeChild) {
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
  module: Edit,
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
  module: Edit,
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
  // All node types use this value in the same way to obtain the ID type, but each node does so separately because we
  // must pop the tree's span from the ID map *after* processing children.
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

type NodeKey = string
type TokenKey = string
function nodeKey(start: number, length: number, type: RawAst.Tree.Type | undefined): NodeKey {
  const type_ = type?.toString() ?? '?'
  return `${start}:${length}:${type_}`
}
function tokenKey(start: number, length: number): TokenKey {
  return `${start}:${length}`
}

type NodeSpanMap = Map<NodeKey, AstId[]>
type TokenSpanMap = Map<TokenKey, TokenId>
export type InfoMap = {
  nodes: NodeSpanMap
  tokens: TokenSpanMap
}

type PrintedSource = {
  info: InfoMap
  code: string
}

/** Return stringification with associated ID map. This is only exported for testing. */
export function print(ast: Ast): PrintedSource {
  const info: InfoMap = {
    nodes: new Map(),
    tokens: new Map(),
  }
  const code = ast._print(info, 0, '')
  return { info, code }
}

type DebugTree = (DebugTree | string)[]
export function debug(root: Ast, universe?: Map<AstId, Ast>): DebugTree {
  const module = root.module
  return Array.from(root.concreteChildren(), (child) => {
    if (child.node instanceof Token) {
      return child.node.code()
    } else {
      const node = module.get(child.node)
      return node ? debug(node, universe) : '<missing>'
    }
  })
}

// FIXME: We should use alias analysis to handle ambiguous names correctly.
export function findModuleMethod(module: Committed, name: string): Function | null {
  for (const node of module.nodes.values()) {
    if (node instanceof Function) {
      if (node.name && node.name.code() === name) {
        return node
      }
    }
  }
  return null
}

export function functionBlock(module: Committed, name: string): BodyBlock | null {
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
  const astExtended = new Map<AstId, RawAstExtended>()
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
  newRoot.module.astExtended = astExtended
  return newRoot
}

export function parse(source: PrintedSource | string): Ast {
  return Ast.parse(source)
}

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
