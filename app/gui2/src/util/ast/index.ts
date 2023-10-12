import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { parse } from '@/util/ffi'
import { LazyObject, debug, validateSpans } from '@/util/parserSupport'
import { assert } from '../assert'

export { Ast }

export function parseEnso(code: string): Ast.Tree {
  const blob = parse(code)
  return Ast.Tree.read(new DataView(blob.buffer), blob.byteLength - 4)
}

/** Read a single line of code
 *
 * Is meant to be a helper for tests. If the code is multilined, an exception is raised.
 */
export function parseEnsoLine(code: string): Ast.Tree {
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
export function readAstSpan(node: Tree, code: string): string {
  const leftOffsetbegin = node.whitespaceStartInCodeParsed
  const leftOffsetEnd = leftOffsetbegin + node.whitespaceLengthInCodeParsed
  const end = leftOffsetEnd + node.childrenLengthInCodeParsed
  return code.substring(leftOffsetEnd, end)
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
// TODO[ao] This is a very hacky way of implementing this. The better solution would be to generate
// such a function from parser types schema. See parser-codegen package.
export function* childrenAstNodes(obj: unknown): Generator<Tree> {
  function* astNodeOrChildren(obj: unknown) {
    if (obj instanceof Tree.AbstractBase) {
      yield obj as Tree
    } else {
      yield* childrenAstNodes(obj)
    }
  }
  if (obj == null) return
  const maybeIterable = obj as Record<symbol, unknown>
  const isIterator = typeof maybeIterable[Symbol.iterator] == 'function'
  if (obj instanceof LazyObject) {
    const prototype = Object.getPrototypeOf(obj)
    const descriptors = Object.getOwnPropertyDescriptors(prototype)
    const objWithProp = obj as unknown as Record<string, unknown>

    for (const prop in descriptors) {
      if (descriptors[prop]?.get == null) continue
      const value = objWithProp[prop]
      yield* astNodeOrChildren(value)
    }
  } else if (isIterator) {
    const iterable = obj as Iterable<unknown>
    for (const element of iterable) {
      yield* astNodeOrChildren(element)
    }
  }
}

/** Returns all AST nodes from `root` tree containing given char, starting from the most nested. */
export function* astContainingChar(charIndex: number, root: Tree): Generator<Tree> {
  for (const child of childrenAstNodes(root)) {
    const begin = child.whitespaceStartInCodeParsed + child.whitespaceLengthInCodeParsed
    const end = begin + child.childrenLengthInCodeParsed
    if (charIndex >= begin && charIndex < end) {
      yield* astContainingChar(charIndex, child)
    }
  }
  yield root
}

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
    expect(readAstSpan(ast, code)).toStrictEqual(code)
    assert(ast.type === Tree.Type.BodyBlock)
    const statements = Array.from(ast.statements)

    assert(statements[0]?.expression != null)
    expect(readAstSpan(statements[0].expression, code)).toStrictEqual('Data.read File')
    assert(statements[0].expression.type === Tree.Type.App)
    expect(readAstSpan(statements[0].expression.func, code)).toStrictEqual('Data.read')
    expect(readAstSpan(statements[0].expression.arg, code)).toStrictEqual('File')

    assert(statements[1]?.expression != null)
    expect(readAstSpan(statements[1].expression, code)).toStrictEqual('2 + 3')
    assert(statements[1].expression.type === Tree.Type.OprApp)
    assert(statements[1].expression.lhs != null)
    assert(statements[1].expression.rhs != null)
    assert(statements[1].expression.opr.ok)
    expect(readAstSpan(statements[1].expression.lhs, code)).toStrictEqual('2')
    expect(readTokenSpan(statements[1].expression.opr.value, code)).toStrictEqual('+')
    expect(readAstSpan(statements[1].expression.rhs, code)).toStrictEqual('3')
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
      expect(readAstSpan(child, code)).toBe(expected?.repr)
    }
  })

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
    const astAtPosition = Array.from(astContainingChar(position, ast))
    const resultWithExpected = astAtPosition.map((ast, i) => {
      return { ast, expected: expected[i] }
    })
    for (const { ast, expected } of resultWithExpected) {
      expect(ast.type).toBe(expected?.type)
      expect(readAstSpan(ast, code)).toBe(expected?.repr)
    }
  })
}
