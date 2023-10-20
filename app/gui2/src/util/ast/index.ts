import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import { parse } from '@/util/ffi'
import { LazyObject, LazySequence } from '@/util/parserSupport'
import * as map from 'lib0/map'
import type { ContentRange, ExprId, IdMap } from 'shared/yjsModel'
import { markRaw } from 'vue'
import type { Opt } from '../opt'
import { isResult, mapOk } from '../result'

export { Ast }

export function parseEnso(code: string): Tree {
  const blob = parse(code)
  return Tree.read(new DataView(blob.buffer), blob.byteLength - 4)
}

/** Read a single line of code
 *
 * Is meant to be a helper for tests. If the code is multilined, an exception is raised.
 */
export function parseEnsoLine(code: string): Tree {
  const block = parseEnso(code)
  assert(block.type === Tree.Type.BodyBlock)
  const statemets = block.statements[Symbol.iterator]()
  const firstLine = statemets.next()
  assert(!firstLine.done)
  assert(!!statemets.next().done)
  assert(firstLine.value.expression != null)
  return firstLine.value.expression
}

/**
 * Read span of code reprsented by given AST node.
 *
 * The AST is assumed to be generated from `code` and not modified sice then.
 * Otherwise an unspecified fragment of `code` may be returned.
 */
export function readAstOrTokenSpan(node: Tree | Token, code: string): string {
  const range = parsedTreeOrTokenRange(node)
  return code.substring(range[0], range[1])
}

export function getAstSpan(node: Tree): ContentRange {
  const leftOffsetbegin = node.whitespaceStartInCodeParsed
  const leftOffsetEnd = leftOffsetbegin + node.whitespaceLengthInCodeParsed
  const end = leftOffsetEnd + node.childrenLengthInCodeParsed
  return [leftOffsetEnd, end]
}

/**
 * Read span of code reprsented by given Token.
 *
 * The Token is assumed to be a part of AST generated from `code`.
 */
export function readTokenSpan(token: Token, code: string): string {
  const begin = token.startInCodeBuffer
  const end = begin + token.lengthInCodeBuffer
  return code.substring(begin, end)
}

/**
 * Read direct AST children.
 */
export function childrenAstNodes(obj: LazyObject): Tree[] {
  const children: Tree[] = []
  const visitor = (obj: LazyObject) => {
    if (Tree.isInstance(obj)) children.push(obj)
    else if (!Token.isInstance(obj)) obj.visitChildren(visitor)
  }
  obj.visitChildren(visitor)
  return children
}
export function childrenAstNodesOrTokens(obj: LazyObject): (Tree | Token)[] {
  const children: (Tree | Token)[] = []
  const visitor = (obj: LazyObject) => {
    if (Tree.isInstance(obj) || Token.isInstance(obj)) {
      children.push(obj)
    } else {
      obj.visitChildren(visitor)
    }
  }
  obj.visitChildren(visitor)
  return children
}

/** Returns all AST nodes from `root` tree containing given char, starting from the most nested. */
export function astContainingChar(charIndex: number, root: Tree): Tree[] {
  return treePath(root, (node) => {
    const begin = node.whitespaceStartInCodeParsed + node.whitespaceLengthInCodeParsed
    const end = begin + node.childrenLengthInCodeParsed
    return charIndex >= begin && charIndex < end
  }).reverse()
}

/** Given a predicate, return a path from the root down the tree containing the
 *  first node at each level found to satisfy the predicate. */
function treePath(obj: LazyObject, pred: (node: Tree) => boolean): Tree[] {
  const path: Tree[] = []
  const visitor = (obj: LazyObject) => {
    if (Tree.isInstance(obj)) {
      const isMatch = pred(obj)
      if (isMatch) path.push(obj)
      return obj.visitChildren(visitor) || isMatch
    } else {
      return obj.visitChildren(visitor)
    }
  }
  obj.visitChildren(visitor)
  return path
}

export function findAstWithRange(
  root: Tree | Token,
  range: ContentRange,
): Tree | Token | undefined {
  for (const child of childrenAstNodes(root)) {
    const [begin, end] = parsedTreeOrTokenRange(child)
    if (begin === range[0] && end === range[1]) return child
    if (begin <= range[0] && end >= range[1]) return findAstWithRange(child, range)
  }
}

export function* walkRecursive(node: Tree | Token): Generator<Tree | Token, void, boolean | void> {
  if (false === (yield node)) return
  const stack: Iterator<Tree | Token>[] = [childrenAstNodesOrTokens(node).values()]
  while (stack.length > 0) {
    const next = stack[stack.length - 1]!.next()
    if (next.done) stack.pop()
    else if (false !== (yield next.value)) stack.push(childrenAstNodesOrTokens(next.value).values())
  }
}

function visitGenerator<T, N, R>(generator: Generator<T, R, N>, visit: (value: T) => N): R {
  let next = generator.next()
  while (!next.done) next = generator.next(visit(next.value))
  return next.value
}

/**
 * Recursively visit AST nodes in depth-first order. The children of a node will be skipped when
 * `visit` callback returns `false`.
 *
 * @param node Root node of the tree to walk. It will be visited first.
 * @param visit Callback that is called for each node. If it returns `false`, the children of that
 * node will be skipped, and the walk will continue to the next sibling.
 */
export function visitRecursive(node: Tree | Token, visit: (node: Tree | Token) => boolean) {
  visitGenerator(walkRecursive(node), visit)
}

export function parsedTreeRange(tree: Tree): ContentRange {
  const start = tree.whitespaceStartInCodeParsed + tree.whitespaceLengthInCodeParsed
  const end = start + tree.childrenLengthInCodeParsed
  return [start, end]
}

export function parsedTokenRange(token: Token): ContentRange {
  const start = token.startInCodeBuffer
  const end = start + token.lengthInCodeBuffer
  return [start, end]
}

export function parsedTreeOrTokenRange(node: Tree | Token): ContentRange {
  if (node instanceof Tree.AbstractBase) return parsedTreeRange(node)
  else return parsedTokenRange(node)
}

export function debug(obj: unknown): unknown {
  if (obj instanceof LazyObject) {
    const proto = Object.getPrototypeOf(obj)
    const fields = Object.fromEntries(allGetterNames(obj).map((k) => [k, debug((obj as any)[k])]))
    if (Object.hasOwnProperty.call(obj, 'type')) {
      const className = proto?.constructor?.name
      return { type: className, ...fields }
    } else {
      return fields
    }
  } else if (obj instanceof LazySequence) {
    return Array.from(obj, debug)
  } else if (isResult(obj)) {
    return mapOk(obj, debug)
  } else {
    return obj
  }
}

const protoGetters = new Map()
function allGetterNames(obj: object): string[] {
  let proto = Object.getPrototypeOf(obj)
  return map.setIfUndefined(protoGetters, proto, () => {
    const props = new Map<string, PropertyDescriptor>()
    do {
      for (const [name, prop] of Object.entries(Object.getOwnPropertyDescriptors(proto))) {
        if (!props.has(name)) props.set(name, prop)
      }
    } while ((proto = Object.getPrototypeOf(proto)))
    const getters = new Set<string>()
    for (const [name, prop] of props.entries()) {
      if (prop.get != null && prop.configurable && !debugHideFields.includes(name)) {
        getters.add(name)
      }
    }
    return [...getters]
  })
}

const debugHideFields = [
  '_v',
  '__proto__',
  'codeReprBegin',
  'codeReprLen',
  'leftOffsetCodeReprBegin',
  'leftOffsetCodeReprLen',
  'leftOffsetVisible',
  'spanLeftOffsetCodeReprBegin',
  'spanLeftOffsetCodeReprLen',
  'spanLeftOffsetVisible',
]

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  // Not working cases commented.
  const parseCases = [
    ' foo bar\n',
    'Data.read\n2 + 2',
    'Data.read File\n2 + 3',
    // 'Data.read "File"\n2 + 3',
    'foo bar=baz',
    // '2\n + 3\n + 4',
  ]

  test.each(parseCases)("Parsing '%s'", (code) => {
    expect(debug(parseEnso(code))).toMatchSnapshot()
  })

  test.each(parseCases)("AST spans of '%s' are valid", (input) => {
    const tree = parseEnso(input)
    const endPos = validateSpans(tree)
    expect(endPos).toStrictEqual(input.length)
  })

  test("Reading AST node's code", () => {
    const code = 'Data.read File\n2 + 3'
    const ast = parseEnso(code)
    expect(readAstOrTokenSpan(ast, code)).toStrictEqual(code)
    assert(ast.type === Tree.Type.BodyBlock)
    const statements = Array.from(ast.statements)

    assert(statements[0]?.expression != null)
    expect(readAstOrTokenSpan(statements[0].expression, code)).toStrictEqual('Data.read File')
    assert(statements[0].expression.type === Tree.Type.App)
    expect(readAstOrTokenSpan(statements[0].expression.func, code)).toStrictEqual('Data.read')
    expect(readAstOrTokenSpan(statements[0].expression.arg, code)).toStrictEqual('File')

    assert(statements[1]?.expression != null)
    expect(readAstOrTokenSpan(statements[1].expression, code)).toStrictEqual('2 + 3')
    assert(statements[1].expression.type === Tree.Type.OprApp)
    assert(statements[1].expression.lhs != null)
    assert(statements[1].expression.rhs != null)
    assert(statements[1].expression.opr.ok)
    expect(readAstOrTokenSpan(statements[1].expression.lhs, code)).toStrictEqual('2')
    expect(readTokenSpan(statements[1].expression.opr.value, code)).toStrictEqual('+')
    expect(readAstOrTokenSpan(statements[1].expression.rhs, code)).toStrictEqual('3')
  })

  test.each([
    [
      '2 + a',
      [
        { type: Tree.Type.Number, repr: '2' },
        { type: Tree.Type.Ident, repr: 'a' },
      ],
    ],
    [
      'a.b',
      [
        { type: Tree.Type.Ident, repr: 'a' },
        { type: Tree.Type.Ident, repr: 'b' },
      ],
    ],
    [
      'Data.read foo',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'foo' },
      ],
    ],
    ['(2 + a)', [{ type: Tree.Type.OprApp, repr: '2 + a' }]],
    [
      'Data.read\n  foo\n  bar',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'foo' },
        { type: Tree.Type.Ident, repr: 'bar' },
      ],
    ],
    [
      'Data.read file=foo',
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.Ident, repr: 'foo' },
      ],
    ],
  ])("Reading children of '%s'", (code, expected) => {
    const ast = parseEnsoLine(code)
    const children = Array.from(childrenAstNodes(ast))
    const childrenWithExpected = children.map((child, i) => {
      return { child, expected: expected[i] }
    })
    for (const { child, expected } of childrenWithExpected) {
      expect(child.type).toBe(expected?.type)
      expect(readAstOrTokenSpan(child, code)).toBe(expected?.repr)
    }
  })

  // test.each([
  //   [
  //     '2 + a',
  //     [
  //       { type: Tree.Type.Number, repr: '2' },
  //       { type: Tree.Type.Ident, repr: 'a' },
  //     ],
  //   ],
  // ])("Walking AST of '%s'", (code, expected) => {
  //   const ast = parseEnsoLine(code)
  //   const visited = [...walkRecursive(ast)]
  //   expect(visited).toStrictEqual(expected)
  // })

  test.each([
    [
      '2 + a',
      0,
      [
        { type: Tree.Type.Number, repr: '2' },
        { type: Tree.Type.OprApp, repr: '2 + a' },
        { type: Tree.Type.BodyBlock, repr: '2 + a' },
      ],
    ],
    [
      'Data.read foo',
      5,
      [
        { type: Tree.Type.Ident, repr: 'read' },
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
    [
      'Data.read foo',
      4,
      [
        { type: Tree.Type.OprApp, repr: 'Data.read' },
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
    [
      'Data.read foo',
      9,
      [
        { type: Tree.Type.App, repr: 'Data.read foo' },
        { type: Tree.Type.BodyBlock, repr: 'Data.read foo' },
      ],
    ],
    [
      'Data.',
      4,
      [
        { type: Tree.Type.OprApp, repr: 'Data.' },
        { type: Tree.Type.OprSectionBoundary, repr: 'Data.' },
        { type: Tree.Type.BodyBlock, repr: 'Data.' },
      ],
    ],
  ])("Reading AST from code '%s' and position %i", (code, position, expected) => {
    const ast = parseEnso(code)
    const astAtPosition = astContainingChar(position, ast)
    const resultWithExpected = astAtPosition.map((ast, i) => {
      return { ast, expected: expected[i] }
    })
    for (const { ast, expected } of resultWithExpected) {
      expect(ast.type).toBe(expected?.type)
      expect(readAstOrTokenSpan(ast, code)).toBe(expected?.repr)
    }
  })
}

/**
 * AST with additional metadata containing AST IDs and original code reference. Can only be
 * constructed by parsing a full module code.
 */
export class AstExtended<T extends Tree | Token = Tree | Token, HasIdMap extends boolean = true> {
  inner: T
  private ctx: AstExtendedCtx<HasIdMap>

  public static parse(moduleCode: string): AstExtended<Tree, false>
  public static parse(moduleCode: string, idMap: IdMap): AstExtended<Tree, true>
  public static parse(moduleCode: string, idMap?: IdMap): AstExtended<Tree, boolean> {
    const ast = parseEnso(moduleCode)
    if (idMap != null) {
      visitRecursive(ast, (node) => {
        const range = parsedTreeOrTokenRange(node)
        idMap.getOrInsertUniqueId(range)
        return true
      })
    }

    const ctx = new AstExtendedCtx(moduleCode, idMap)
    return new AstExtended(ast, ctx)
  }

  treeTypeName(): (typeof Tree.typeNames)[number] | null {
    return Tree.isInstance(this.inner) ? Tree.typeNames[this.inner.type] : null
  }

  isToken<T extends Ast.Token.Type>(
    type?: T,
  ): this is AstExtended<Extract<Ast.Token, { type: T }>, HasIdMap> {
    return Token.isInstance(this.inner) && (type == null || this.inner.type === type)
  }

  isTree<T extends Ast.Tree.Type>(
    type?: T,
  ): this is AstExtended<Extract<Ast.Tree, { type: T }>, HasIdMap> {
    return Tree.isInstance(this.inner) && (type == null || this.inner.type === type)
  }

  private constructor(tree: T, ctx: AstExtendedCtx<HasIdMap>) {
    markRaw(this)
    this.inner = tree
    this.ctx = ctx
  }

  get astId(): CondType<ExprId, HasIdMap> {
    if (this.ctx.idMap != null) {
      const id = this.ctx.idMap.getIfExist(parsedTreeOrTokenRange(this.inner))
      assert(id != null, 'All AST nodes should have an assigned ID')
      return id as CondType<ExprId, HasIdMap>
    } else {
      return undefined as CondType<ExprId, HasIdMap>
    }
  }

  debug(): unknown {
    return debug(this.inner)
  }

  tryMap<T2 extends Tree>(mapper: (t: T) => Opt<T2>): AstExtended<T2, HasIdMap> | undefined {
    const mapped = mapper(this.inner)
    if (mapped == null) return
    return new AstExtended(mapped, this.ctx)
  }

  map<T2 extends Tree | Token>(mapper: (t: T) => T2): AstExtended<T2, HasIdMap> {
    return new AstExtended(mapper(this.inner), this.ctx)
  }

  *visit<T2 extends Tree | Token>(
    visitor: (t: T) => Generator<T2>,
  ): Generator<AstExtended<T2, HasIdMap>> {
    for (const child of visitor(this.inner)) {
      yield new AstExtended(child, this.ctx)
    }
  }

  repr() {
    return readAstOrTokenSpan(this.inner, this.ctx.parsedCode)
  }

  span(): ContentRange {
    return parsedTreeOrTokenRange(this.inner)
  }

  children(): AstExtended<Tree | Token, HasIdMap>[] {
    return childrenAstNodesOrTokens(this.inner).map((child) => new AstExtended(child, this.ctx))
  }

  walkRecursive(): Generator<AstExtended<Tree | Token, HasIdMap>> {
    return this.visit(walkRecursive)
  }

  whitespaceLength() {
    return 'whitespaceLengthInCodeBuffer' in this.inner
      ? this.inner.whitespaceLengthInCodeBuffer
      : this.inner.whitespaceLengthInCodeParsed
  }

  visitRecursive(visitor: (t: AstExtended<Tree | Token, HasIdMap>) => boolean) {
    visitGenerator(this.walkRecursive(), visitor)
  }
}

type CondType<T, Cond extends boolean> = Cond extends true
  ? T
  : Cond extends false
  ? undefined
  : T | undefined

class AstExtendedCtx<HasIdMap extends boolean> {
  parsedCode: string
  idMap: CondType<IdMap, HasIdMap>
  constructor(parsedCode: string, idMap: CondType<IdMap, HasIdMap>) {
    this.parsedCode = parsedCode
    this.idMap = idMap
  }
}

function validateSpans(obj: LazyObject, initialPos?: number): number {
  const state = { pos: initialPos ?? 0 }
  const visitor = (value: LazyObject) => {
    if (
      Token.isInstance(value) &&
      !(value.whitespaceLengthInCodeBuffer + value.lengthInCodeBuffer === 0)
    ) {
      assert(value.whitespaceStartInCodeBuffer === state.pos)
      state.pos += value.whitespaceLengthInCodeBuffer
      assert(value.startInCodeBuffer === state.pos)
      state.pos += value.lengthInCodeBuffer
    } else if (Tree.isInstance(value)) {
      assert(value.whitespaceStartInCodeParsed === state.pos)
      state.pos += value.whitespaceLengthInCodeParsed
      const end = state.pos + value.childrenLengthInCodeParsed
      value.visitChildren(visitor)
      assert(state.pos === end)
    } else {
      value.visitChildren(visitor)
    }
  }
  visitor(obj)
  return state.pos
}
