import {
  astContainingChar,
  childrenAstNodes,
  debugAst,
  parseEnso,
  parseEnsoLine,
  readAstOrTokenSpan,
  readAstSpan,
  readTokenSpan,
  walkRecursive,
} from '@/util/ast'
import { initializeFFI } from 'shared/ast/ffi'
import { Token, Tree } from 'shared/ast/generated/ast'
import type { LazyObject } from 'shared/ast/parserSupport'
import { assert, expect, test } from 'vitest'

await initializeFFI()

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

const parseCases = [
  'foo bar\n',
  'Data.read\n2 + 2',
  'Data.read File\n2 + 3',
  'Data.read "File"\n2 + 3',
  'foo bar=baz',
  '2\n + 3\n + 4',
]

test.each(parseCases)("Parsing '%s'", (code) => {
  expect(debugAst(parseEnso(code))).toMatchSnapshot()
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
    [
      { tree: Tree.Type.OprApp, repr: '2 + a' },
      { tree: Tree.Type.Number, repr: '2' },
      { token: Token.Type.Digits, repr: '2' },
      { token: Token.Type.Operator, repr: '+' },
      { tree: Tree.Type.Ident, repr: 'a' },
      { token: Token.Type.Ident, repr: 'a' },
    ],
  ],
])("Walking AST of '%s'", (code, expected) => {
  const ast = parseEnsoLine(code)
  const visited = Array.from(walkRecursive(ast))
  const visitedRepr = visited.map((visited) => {
    return {
      [Tree.isInstance(visited) ? 'tree' : 'token']: visited.type,
      repr: readAstOrTokenSpan(visited, code),
    }
  })

  expect(visitedRepr).toStrictEqual(expected)
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
