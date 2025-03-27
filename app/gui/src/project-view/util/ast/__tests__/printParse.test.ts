import { assertDefined } from '@/util/assert'
import { Ast } from '@/util/ast'
import { expect, test } from 'vitest'

const parseCases = [
  { code: 'foo bar+baz', tree: [['foo'], [['bar'], '+', ['baz']]] },
  { code: '(foo)', tree: ['(', ['foo'], ')'] },
]
test.each(parseCases)('parse: %s', (testCase) => {
  const root = Ast.parseExpression(testCase.code)
  assertDefined(root)
  expect(Ast.tokenTree(root)).toEqual(testCase.tree)
})
