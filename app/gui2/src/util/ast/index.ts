import * as Ast from '@/generated/ast'
import { Token, Tree } from '@/generated/ast'
import { assert } from '@/util/assert'
import { parse } from '@/util/ffi'
import { LazyObject, debug } from '@/util/parserSupport'

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
  const statements = block.statements[Symbol.iterator]()
  const firstLine = statements.next()
  assert(!firstLine.done)
  assert(!!statements.next().done)
  assert(firstLine.value.expression != null)
  return firstLine.value.expression
}

/**
 * Read ast span information in `String.substring` compatible way. The returned span does not
 * include left whitespace offset.
 *
 * @returns Object with `start` and `end` properties; index of first character in the `node`
 *   and first character _not_ being in the `node`.
 */
export function astSpan(node: Tree): { start: number; end: number } {
  const start = node.whitespaceStartInCodeParsed + node.whitespaceLengthInCodeParsed
  const end = start + node.childrenLengthInCodeParsed
  return { start, end }
}

/**
 * Read span of code reprsented by given AST node, not including left whitespace offset.
 *
 * The AST is assumed to be generated from `code` and not modified sice then.
 * Otherwise an unspecified fragment of `code` may be returned.
 */
export function readAstSpan(node: Tree, code: string): string {
  const { start, end } = astSpan(node)
  return code.substring(start, end)
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
    if (Tree.isInstance(obj)) {
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

if (import.meta.vitest) {
  const { test, expect } = import.meta.vitest

  const parseCases = [
    'foo bar\n',
    'Data.read\n2 + 2',
    'Data.read File\n2 + 3',
    'Data.read "File"\n2 + 3',
    'foo bar=baz',
    '2\n + 3\n + 4',
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
    ['(', [{ type: Tree.Type.Invalid, repr: '(' }]],
    [
      '(foo',
      [
        { type: Tree.Type.Invalid, repr: '(' },
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
    const astAtPosition = astContainingChar(position, ast)
    const resultWithExpected = astAtPosition.map((ast, i) => {
      return { ast, expected: expected[i] }
    })
    for (const { ast, expected } of resultWithExpected) {
      expect(ast.type).toBe(expected?.type)
      expect(readAstSpan(ast, code)).toBe(expected?.repr)
    }
  })
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
