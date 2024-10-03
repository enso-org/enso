import { assert } from '@/util/assert'
import { RawAstExtended } from '@/util/ast/extended'
import { GeneralOprApp, operandsOfLeftAssocOprChain, type OperatorChain } from '@/util/ast/opr'
import { RawAst } from '@/util/ast/raw'
import { expect, test } from 'vitest'

test.each([
  { code: '2 + 3', result: ['2', '+', '3'] },
  { code: '2 + 4 + 5', result: ['2 + 4', '+', '5'] },
  { code: '2\n + 3\n + 4', result: ['2', '+', '3', '+', '4'] },
  { code: '2\n - 4\n * 5', result: ['2', '-', '4', '*', '5'] },
  { code: 'foo . bar\n . baz', result: ['foo . bar', '.', 'baz'] },
  // See https://github.com/orgs/enso-org/discussions/8021
  // { code: '2 + 3\n + 4', result: ['2 + 3', '+', '4'] },
  { code: '+ 2', result: [null, '+', '2'] },
  { code: '2 +', result: ['2', '+', null] },
  { code: '.foo', result: [null, '.', 'foo'] },
  { code: 'foo.', result: ['foo', '.', null] },
])('Generalized infix from $code', ({ code, result }) => {
  let ast = RawAstExtended.parseLine(code)
  if (ast.isTree(RawAst.Tree.Type.OprSectionBoundary)) {
    ast = ast.map((boundary) => boundary.ast)
  }
  assert(
    ast.isTree(RawAst.Tree.Type.OprApp) || ast.isTree(RawAst.Tree.Type.OperatorBlockApplication),
  )
  const opr = new GeneralOprApp(ast as OperatorChain<false>)
  expect(Array.from(opr.componentsReprs())).toStrictEqual(result)
})

test.each([
  {
    code: '2 + 3',
    result: [
      { type: 'ast', repr: '2' },
      { type: 'ast', repr: '3' },
    ],
  },
  {
    code: '2 + 3 + 4',
    result: [
      { type: 'ast', repr: '2' },
      { type: 'ast', repr: '3' },
      { type: 'ast', repr: '4' },
    ],
  },
  {
    code: '2 * 3 + 4',
    result: [
      { type: 'ast', repr: '2 * 3' },
      { type: 'ast', repr: '4' },
    ],
  },
  {
    code: '2\n + 3\n + 4',
    result: [
      { type: 'ast', repr: '2' },
      { type: 'ast', repr: '3' },
      { type: 'ast', repr: '4' },
    ],
  },
  // See https://github.com/orgs/enso-org/discussions/8021
  // {
  //   code: '2 + 3\n + 4',
  //   result: [
  //     { type: 'ast', repr: '2' },
  //     { type: 'ast', repr: '3' },
  //     { type: 'ast', repr: '4' },
  //   ],
  // },
  // There is a bug in AST spans in some OperatorBlockApplications. Fix this test once fixed
  {
    code: '2\n * 3\n + 44',
    result: [
      { type: 'partOfOprBlockApp', repr: '2\n * 3\n + 44', statements: 1 },
      { type: 'ast', repr: '44' },
    ],
  },
  {
    code: '2\n + 3\n * 4\n + 55',
    result: [
      { type: 'partOfOprBlockApp', repr: '2\n + 3\n * 4\n + 55', statements: 2 },
      { type: 'ast', repr: '55' },
    ],
  },
  // https://github.com/orgs/enso-org/discussions/8021
  // {
  //   code: '2 * 3\n + 4',
  //   result: [
  //     { type: 'ast', repr: '2 * 3' },
  //     { type: 'ast', repr: '4' },
  //   ],
  // },
  {
    code: 'foo bar',
    result: [{ type: 'ast', repr: 'foo bar' }],
  },
  {
    code: '2 * 3',
    opr: '+',
    result: [{ type: 'ast', repr: '2 * 3' }],
  },
])(
  'Getting left-associative operator operands in $code',
  ({
    code,
    opr,
    result,
  }: {
    code: string
    opr?: string
    result: { type: string; repr: string; statements?: number }[]
  }) => {
    const ast = RawAstExtended.parseLine(code)
    const actual = operandsOfLeftAssocOprChain(ast, opr)
    const actualWithExpected = Array.from(actual, (operand, i) => {
      return { actual: operand, expected: result[i] }
    })
    for (const { actual, expected } of actualWithExpected) {
      if (expected === null) {
        expect(actual).toBeNull()
      } else {
        expect(actual?.type).toStrictEqual(expected?.type)
        if (actual?.type === 'ast') {
          expect(actual.ast.repr()).toStrictEqual(expected?.repr)
        } else {
          assert(actual?.type == 'partOfOprBlockApp')
          expect(actual.ast.repr()).toStrictEqual(expected?.repr)
          expect(actual.statements).toStrictEqual(expected?.statements)
        }
      }
    }
  },
)
