import { RawAst, rawParseModule, readAstOrTokenSpan, walkRecursive } from '@/util/ast/raw'
import * as iter from 'enso-common/src/utilities/data/iter'
import { assert, expect, test } from 'vitest'
import { Token, Tree } from 'ydoc-shared/ast/generated/ast'
import type { LazyObject } from 'ydoc-shared/ast/parserSupport'
import { assertDefined } from 'ydoc-shared/util/assert'

/**
 * Read a single line of code
 *
 * Helper for tests. If the code is multiline, an exception is raised.
 */
function rawParseLine(code: string): RawAst.Tree {
  const block = rawParseModule(code)
  const soleExpression = iter.tryGetSoleValue(block.statements)?.expression
  assertDefined(soleExpression)
  return soleExpression
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

const parseCases = [
  'foo bar\n',
  'Data.read\n2 + 2',
  'Data.read File\n2 + 3',
  'Data.read "File"\n2 + 3',
  'foo bar=baz',
  '2\n + 3\n + 4',
]

test.each(parseCases)("AST spans of '%s' are valid", (input) => {
  const tree = rawParseModule(input)
  const endPos = validateSpans(tree)
  expect(endPos).toStrictEqual(input.length)
})

test.each([
  [
    '2 + a',
    [
      { tree: Tree.Type.ExpressionStatement, repr: '2 + a' },
      { tree: Tree.Type.OprApp, repr: '2 + a' },
      { tree: Tree.Type.Number, repr: '2' },
      { token: Token.Type.Digits, repr: '2' },
      { token: Token.Type.Operator, repr: '+' },
      { tree: Tree.Type.Ident, repr: 'a' },
      { token: Token.Type.Ident, repr: 'a' },
    ],
  ],
])("Walking AST of '%s'", (code, expected) => {
  const ast = rawParseLine(code)
  const visited = Array.from(walkRecursive(ast))
  const visitedRepr = visited.map((visited) => {
    return {
      [Tree.isInstance(visited) ? 'tree' : 'token']: visited.type,
      repr: readAstOrTokenSpan(visited, code),
    }
  })

  expect(visitedRepr).toStrictEqual(expected)
})
