import * as iter from 'enso-common/src/utilities/data/iter'
import * as map from 'lib0/map'
import * as Y from 'yjs'
import { assert } from '../util/assert'
import type { IdMap } from '../yjsModel'
import { abstractMarkdown } from './documentation'
import { parse_block, parse_module } from './ffi'
import * as RawAst from './generated/ast'
import type { NodeKey, NodeSpanMap, SpanMap, TokenSpanMap } from './idMap'
import { nodeKey, tokenKey } from './idMap'
import { MutableModule } from './mutableModule'
import type { LazyObject } from './parserSupport'
import { Token } from './token'
import type {
  Ast,
  FunctionDefFields,
  MutableBodyBlock,
  MutableExpression,
  MutableStatement,
  NodeChild,
  Owned,
  OwnedRefs,
  TextElement,
  TextToken,
} from './tree'
import {
  App,
  asOwned,
  Assignment,
  AutoscopedIdentifier,
  BodyBlock,
  ExpressionStatement,
  FunctionDef,
  Generic,
  Group,
  Ident,
  Import,
  Invalid,
  MutableExpressionStatement,
  MutableIdent,
  MutableInvalid,
  NegationApp,
  NumericLiteral,
  OprApp,
  parentId,
  PropertyAccess,
  TextLiteral,
  UnaryOprApp,
  Vector,
  Wildcard,
} from './tree'

/** Return the raw parser output for the given code, parsed as a module. */
export function rawParseModule(code: string): RawAst.Tree.BodyBlock {
  return deserializeBlock(parse_module(code))
}

/** Return the raw parser output for the given code, parsed as a body block. */
export function rawParseBlock(code: string): RawAst.Tree.BodyBlock {
  return deserializeBlock(parse_block(code))
}

function deserializeBlock(blob: Uint8Array): RawAst.Tree.BodyBlock {
  const tree = RawAst.Tree.read(new DataView(blob.buffer), blob.byteLength - 4)
  // The root of the parser output is always a body block.
  assert(tree.type === RawAst.Tree.Type.BodyBlock)
  return tree
}

/** Produce `Ast` types from `RawAst` parser output. */
export function abstract(
  module: MutableModule,
  tree: RawAst.Tree.BodyBlock,
  code: string,
  substitutor?: (key: NodeKey) => Owned | undefined,
): { root: Owned<MutableBodyBlock>; spans: SpanMap }
export function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  substitutor?: (key: NodeKey) => Owned | undefined,
): { root: Owned; spans: SpanMap }
/** Implementation of `abstract`. */
export function abstract(
  module: MutableModule,
  tree: RawAst.Tree,
  code: string,
  substitutor?: (key: NodeKey) => Owned | undefined,
): { root: Owned; spans: SpanMap } {
  const abstractor = new Abstractor(module, code, substitutor)
  const root = abstractor.abstractTree(tree).node
  const spans = { tokens: abstractor.tokens, nodes: abstractor.nodes }
  return { root: root as Owned<MutableBodyBlock>, spans }
}

/** Produces `Ast` types from `RawAst` parser output. */
class Abstractor {
  private readonly module: MutableModule
  private readonly code: string
  private readonly substitutor: ((key: NodeKey) => Owned | undefined) | undefined
  readonly nodes: NodeSpanMap
  readonly tokens: TokenSpanMap

  /**
   *  @param module - Where to allocate the new nodes.
   *  @param code - Source code that will be used to resolve references in any passed `RawAst` objects.
   *  @param substitutor - A function that can inject subtrees for some spans, instead of the abstractor producing them.
   *    This can be used for incremental abstraction.
   */
  constructor(
    module: MutableModule,
    code: string,
    substitutor?: (key: NodeKey) => Owned | undefined,
  ) {
    this.module = module
    this.code = code
    this.substitutor = substitutor
    this.nodes = new Map()
    this.tokens = new Map()
  }

  abstractStatement(tree: RawAst.Tree): {
    whitespace: string | undefined
    node: Owned<MutableStatement>
  } {
    return this.abstractTree(tree) as any
  }

  abstractExpression(tree: RawAst.Tree): {
    whitespace: string | undefined
    node: Owned<MutableExpression>
  } {
    return this.abstractTree(tree) as any
  }

  abstractTree(tree: RawAst.Tree): { whitespace: string | undefined; node: Owned } {
    const whitespaceStart = tree.whitespaceStartInCodeParsed
    const whitespaceEnd = whitespaceStart + tree.whitespaceLengthInCodeParsed
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd)
    const codeStart = whitespaceEnd
    const codeEnd = codeStart + tree.childrenLengthInCodeParsed
    const spanKey = nodeKey(codeStart, codeEnd - codeStart)
    const substitute = this.substitutor?.(spanKey)
    if (substitute) return { node: substitute, whitespace }
    let node: Owned
    switch (tree.type) {
      case RawAst.Tree.Type.BodyBlock: {
        const lines = Array.from(tree.statements, (line) => {
          const newline = this.abstractToken(line.newline)
          const statement = line.expression ? this.abstractStatement(line.expression) : undefined
          return { newline, statement }
        })
        node = BodyBlock.concrete(this.module, lines)
        break
      }
      case RawAst.Tree.Type.Function: {
        node = this.abstractFunction(tree)
        break
      }
      case RawAst.Tree.Type.Ident: {
        const token = this.abstractToken(tree.token)
        node = Ident.concrete(this.module, token)
        break
      }
      case RawAst.Tree.Type.Assignment: {
        const docLine = tree.docLine && this.abstractDocLine(tree.docLine)
        const pattern = this.abstractExpression(tree.pattern)
        const equals = this.abstractToken(tree.equals)
        const value = this.abstractExpression(tree.expr)
        node = Assignment.concrete(this.module, docLine, pattern, equals, value)
        break
      }
      case RawAst.Tree.Type.App: {
        const func = this.abstractExpression(tree.func)
        const arg = this.abstractExpression(tree.arg)
        node = App.concrete(this.module, func, undefined, undefined, arg)
        break
      }
      case RawAst.Tree.Type.NamedApp: {
        const func = this.abstractExpression(tree.func)
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const name = this.abstractToken(tree.name)
        const equals = this.abstractToken(tree.equals)
        const arg = this.abstractExpression(tree.arg)
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        const parens = open && close ? { open, close } : undefined
        const nameSpecification = { name, equals }
        node = App.concrete(this.module, func, parens, nameSpecification, arg)
        break
      }
      case RawAst.Tree.Type.UnaryOprApp: {
        const opr = this.abstractToken(tree.opr)
        const arg = tree.rhs ? this.abstractExpression(tree.rhs) : undefined
        if (arg && opr.node.code() === '-') {
          node = NegationApp.concrete(this.module, opr, arg)
        } else {
          node = UnaryOprApp.concrete(this.module, opr, arg)
        }
        break
      }
      case RawAst.Tree.Type.AutoscopedIdentifier: {
        const opr = this.abstractToken(tree.opr)
        const ident = this.abstractToken(tree.ident)
        node = AutoscopedIdentifier.concrete(this.module, opr, ident)
        break
      }
      case RawAst.Tree.Type.OprApp: {
        const lhs = tree.lhs ? this.abstractExpression(tree.lhs) : undefined
        const opr =
          tree.opr.ok ?
            [this.abstractToken(tree.opr.value)]
          : Array.from(tree.opr.error.payload.operators, this.abstractToken.bind(this))
        const rhs = tree.rhs ? this.abstractExpression(tree.rhs) : undefined
        const soleOpr = iter.tryGetSoleValue(opr)
        if (soleOpr?.node.code() === '.' && rhs?.node instanceof MutableIdent) {
          // Propagate type.
          const rhs_ = { ...rhs, node: rhs.node }
          node = PropertyAccess.concrete(this.module, lhs, soleOpr, rhs_)
        } else {
          node = OprApp.concrete(this.module, lhs, opr, rhs)
        }
        break
      }
      case RawAst.Tree.Type.Number: {
        const tokens = []
        if (tree.base) tokens.push(this.abstractToken(tree.base))
        if (tree.integer) tokens.push(this.abstractToken(tree.integer))
        if (tree.fractionalDigits) {
          tokens.push(this.abstractToken(tree.fractionalDigits.dot))
          tokens.push(this.abstractToken(tree.fractionalDigits.digits))
        }
        node = NumericLiteral.concrete(this.module, tokens)
        break
      }
      case RawAst.Tree.Type.Wildcard: {
        const token = this.abstractToken(tree.token)
        node = Wildcard.concrete(this.module, token)
        break
      }
      // These expression types are (or will be) used for backend analysis.
      // The frontend can ignore them, avoiding some problems with expressions sharing spans
      // (which makes it impossible to give them unique IDs in the current IdMap format).
      case RawAst.Tree.Type.OprSectionBoundary:
      case RawAst.Tree.Type.TemplateFunction:
        return { whitespace, node: this.abstractExpression(tree.ast).node }
      case RawAst.Tree.Type.Invalid: {
        const expression = this.abstractTree(tree.ast)
        node = Invalid.concrete(this.module, expression)
        break
      }
      case RawAst.Tree.Type.Group: {
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const expression = tree.body ? this.abstractExpression(tree.body) : undefined
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        node = Group.concrete(this.module, open, expression, close)
        break
      }
      case RawAst.Tree.Type.TextLiteral: {
        const open = tree.open ? this.abstractToken(tree.open) : undefined
        const newline = tree.newline ? this.abstractToken(tree.newline) : undefined
        const elements = Array.from(tree.elements, (raw) => this.abstractTextElement(raw))
        const close = tree.close ? this.abstractToken(tree.close) : undefined
        node = TextLiteral.concrete(this.module, open, newline, elements, close)
        break
      }
      case RawAst.Tree.Type.ExpressionStatement: {
        const docLine = tree.docLine && this.abstractDocLine(tree.docLine)
        const expression = this.abstractExpression(tree.expression)
        node = ExpressionStatement.concrete(this.module, docLine, expression)
        break
      }
      case RawAst.Tree.Type.Import: {
        const recurseBody = (tree: RawAst.Tree) => {
          const body = this.abstractExpression(tree)
          if (body.node instanceof MutableInvalid && body.node.code() === '') return undefined
          return body
        }
        const recurseSegment = (segment: RawAst.MultiSegmentAppSegment) => ({
          header: this.abstractToken(segment.header),
          body: segment.body ? recurseBody(segment.body) : undefined,
        })
        const polyglot = tree.polyglot ? recurseSegment(tree.polyglot) : undefined
        const from = tree.from ? recurseSegment(tree.from) : undefined
        const import_ = recurseSegment(tree.import)
        const all = tree.all ? this.abstractToken(tree.all) : undefined
        const as = tree.as ? recurseSegment(tree.as) : undefined
        const hiding = tree.hiding ? recurseSegment(tree.hiding) : undefined
        node = Import.concrete(this.module, polyglot, from, import_, all, as, hiding)
        break
      }
      case RawAst.Tree.Type.Array: {
        const left = this.abstractToken(tree.left)
        const elements = []
        if (tree.first) elements.push({ value: this.abstractExpression(tree.first) })
        else if (!tree.rest.next().done) elements.push({ value: undefined })
        for (const rawElement of tree.rest) {
          elements.push({
            delimiter: this.abstractToken(rawElement.operator),
            value: rawElement.body && this.abstractExpression(rawElement.body),
          })
        }
        const right = this.abstractToken(tree.right)
        node = Vector.concrete(this.module, left, elements, right)
        break
      }
      default: {
        node = Generic.concrete(this.module, this.abstractChildren(tree))
      }
    }
    map.setIfUndefined(this.nodes, spanKey, (): Ast[] => []).unshift(node)
    return { node, whitespace }
  }

  private abstractFunction(tree: RawAst.Tree.Function) {
    const docLine = tree.docLine && this.abstractDocLine(tree.docLine)
    const { markdown: docMarkdown, hash: docLineMarkdownHash } = abstractMarkdown(
      docLine?.docs.elements,
    )
    const annotationLines = Array.from(tree.annotationLines, (anno) => ({
      annotation: {
        operator: this.abstractToken(anno.annotation.operator),
        annotation: this.abstractToken(anno.annotation.annotation),
        argument: anno.annotation.argument && this.abstractExpression(anno.annotation.argument),
      },
      newlines: Array.from(anno.newlines, this.abstractToken.bind(this)),
    }))
    const signatureLine = tree.signatureLine && {
      signature: this.abstractTypeSignature(tree.signatureLine.signature),
      newlines: Array.from(tree.signatureLine.newlines, this.abstractToken.bind(this)),
    }
    const private_ = tree.private && this.abstractToken(tree.private)
    const name = this.abstractExpression(tree.name)
    const argumentDefinitions = Array.from(tree.args, (arg) => ({
      open: arg.open && this.abstractToken(arg.open),
      open2: arg.open2 && this.abstractToken(arg.open2),
      suspension: arg.suspension && this.abstractToken(arg.suspension),
      pattern: this.abstractExpression(arg.pattern),
      type: arg.typeNode && {
        operator: this.abstractToken(arg.typeNode.operator),
        type: this.abstractExpression(arg.typeNode.typeNode),
      },
      close2: arg.close2 && this.abstractToken(arg.close2),
      defaultValue: arg.default && {
        equals: this.abstractToken(arg.default.equals),
        expression: this.abstractExpression(arg.default.expression),
      },
      close: arg.close && this.abstractToken(arg.close),
    }))
    const equals = this.abstractToken(tree.equals)
    const body = tree.body !== undefined ? this.abstractExpression(tree.body) : undefined
    return FunctionDef.concrete(this.module, {
      docLine,
      docLineMarkdownHash,
      docMarkdown: new Y.Text(docMarkdown),
      annotationLines,
      signatureLine,
      private_,
      name,
      argumentDefinitions,
      equals,
      body,
    } satisfies FunctionDefFields<OwnedRefs>)
  }

  private abstractToken(token: RawAst.Token): { whitespace: string; node: Token } {
    const whitespaceStart = token.whitespaceStartInCodeBuffer
    const whitespaceEnd = whitespaceStart + token.whitespaceLengthInCodeBuffer
    const whitespace = this.code.substring(whitespaceStart, whitespaceEnd)
    const codeStart = token.startInCodeBuffer
    const codeEnd = codeStart + token.lengthInCodeBuffer
    const tokenCode = this.code.substring(codeStart, codeEnd)
    const key = tokenKey(codeStart, codeEnd - codeStart)
    const node = Token.new(tokenCode, token.type)
    this.tokens.set(key, node)
    return { whitespace, node }
  }

  private abstractChildren(tree: LazyObject): (NodeChild<Owned> | NodeChild<Token>)[] {
    const children: (NodeChild<Owned> | NodeChild<Token>)[] = []
    const visitor = (child: LazyObject) => {
      if (RawAst.Tree.isInstance(child)) {
        children.push(this.abstractTree(child))
      } else if (RawAst.Token.isInstance(child)) {
        children.push(this.abstractToken(child))
      } else {
        child.visitChildren(visitor)
      }
    }
    tree.visitChildren(visitor)
    return children
  }

  private abstractTextElement(raw: RawAst.TextElement): TextElement<OwnedRefs> {
    switch (raw.type) {
      case RawAst.TextElement.Type.Newline:
      case RawAst.TextElement.Type.Escape:
      case RawAst.TextElement.Type.Section:
        return this.abstractTextToken(raw)
      case RawAst.TextElement.Type.Splice:
        return {
          type: 'splice',
          open: this.abstractToken(raw.open),
          expression: raw.expression && this.abstractExpression(raw.expression),
          close: this.abstractToken(raw.close),
        }
    }
  }

  private abstractTextToken(raw: RawAst.TextElement): TextToken<OwnedRefs> {
    switch (raw.type) {
      case RawAst.TextElement.Type.Newline:
        return { type: 'token', token: this.abstractToken(raw.newline) }
      case RawAst.TextElement.Type.Escape: {
        const negativeOneU32 = 4294967295
        return {
          type: 'token',
          token: this.abstractToken(raw.token),
          interpreted:
            raw.token.value !== negativeOneU32 ? String.fromCodePoint(raw.token.value) : undefined,
        }
      }
      case RawAst.TextElement.Type.Section:
        return { type: 'token', token: this.abstractToken(raw.text) }
      case RawAst.TextElement.Type.Splice:
        throw new Error('Unreachable: Splice in non-interpolated text field')
    }
  }

  private abstractTypeSignature(signature: RawAst.TypeSignature) {
    return {
      name: this.abstractExpression(signature.name),
      operator: this.abstractToken(signature.operator),
      type: this.abstractExpression(signature.typeNode),
    }
  }

  private abstractDocLine(docLine: RawAst.DocLine) {
    return {
      docs: {
        open: this.abstractToken(docLine.docs.open),
        elements: Array.from(docLine.docs.elements, this.abstractTextToken.bind(this)),
      },
      newlines: Array.from(docLine.newlines, this.abstractToken.bind(this)),
    }
  }
}

/** Parse the input as a complete module. */
export function parseModule(code: string, module?: MutableModule): Owned<MutableBodyBlock> {
  return parseModuleWithSpans(code, module).root
}

/** Parse the input as a body block, not the top level of a module. */
export function parseBlock(code: string, module?: MutableModule): Owned<MutableBodyBlock> {
  const tree = rawParseBlock(code)
  return abstract(module ?? MutableModule.Transient(), tree, code).root
}

/**
 * Parse the input as a statement. If it cannot be parsed as a statement (e.g. it is invalid or a block), returns
 * `undefined`.
 */
export function parseStatement(
  code: string,
  module?: MutableModule,
): Owned<MutableStatement> | undefined {
  const module_ = module ?? MutableModule.Transient()
  const ast = parseBlock(code, module)
  const soleStatement = iter.tryGetSoleValue(ast.statements())
  if (!soleStatement) return
  const parent = parentId(soleStatement)
  if (parent) module_.delete(parent)
  soleStatement.fields.set('parent', undefined)
  return asOwned(soleStatement)
}

/**
 * Parse the input as an expression. If it cannot be parsed as an expression (e.g. it is a statement or block), returns
 * `undefined`.
 */
export function parseExpression(
  code: string,
  module?: MutableModule,
): Owned<MutableExpression> | undefined {
  const module_ = module ?? MutableModule.Transient()
  const ast = parseBlock(code, module)
  const soleStatement = iter.tryGetSoleValue(ast.statements())
  if (!(soleStatement instanceof MutableExpressionStatement)) return undefined
  const expression = soleStatement.expression
  module_.delete(soleStatement.id)
  const parent = parentId(expression)
  if (parent) module_.delete(parent)
  expression.fields.set('parent', undefined)
  return asOwned(expression)
}

/** Parse a module, and return it along with a mapping from source locations to parsed objects. */
export function parseModuleWithSpans(
  code: string,
  module?: MutableModule | undefined,
): { root: Owned<MutableBodyBlock>; spans: SpanMap } {
  const tree = rawParseModule(code)
  return abstract(module ?? MutableModule.Transient(), tree, code)
}

/** Return the number of `Ast`s in the tree, including the provided root. */
export function astCount(ast: Ast): number {
  let count = 0
  ast.visitRecursive((_subtree) => {
    count += 1
  })
  return count
}

/**
 * Apply an `IdMap` to a module, using the given `SpanMap`.
 *  @returns The number of IDs that were assigned from the map.
 */
export function setExternalIds(edit: MutableModule, spans: SpanMap, ids: IdMap): number {
  let astsMatched = 0
  for (const [key, externalId] of ids.entries()) {
    const asts = spans.nodes.get(key as NodeKey)
    if (asts) {
      for (const ast of asts) {
        astsMatched += 1
        const editAst = edit.getVersion(ast)
        if (editAst.externalId !== externalId) editAst.setExternalId(externalId)
      }
    }
  }
  return astsMatched
}

/**
 * Determines the context of `ast`: module root, body block, statement, or expression; parses the given code in the same
 * context.
 */
export function parseInSameContext(
  code: string,
  ast: Ast,
  module?: MutableModule,
): { root: Owned; spans: SpanMap } {
  const rawParsed = rawParseInContext(code, getParseContext(ast))
  return abstract(module ?? MutableModule.Transient(), rawParsed, code)
}

type ParseContext = 'module' | 'block' | 'expression' | 'statement'

function getParseContext(ast: Ast): ParseContext {
  const astModuleRoot = ast.module.root()
  if (ast instanceof BodyBlock) return astModuleRoot && ast.is(astModuleRoot) ? 'module' : 'block'
  return ast.isExpression() ? 'expression' : 'statement'
}

function rawParseInContext(code: string, context: ParseContext): RawAst.Tree {
  if (context === 'module') return rawParseModule(code)
  const block = rawParseBlock(code)
  if (context === 'block') return block
  const statement = iter.tryGetSoleValue(block.statements)?.expression
  if (!statement) return block
  if (context === 'statement') return statement
  if (context === 'expression')
    return statement.type === RawAst.Tree.Type.ExpressionStatement ?
        statement.expression
      : statement
  return context satisfies never
}
