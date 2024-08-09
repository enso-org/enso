import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { nodeFromAst, primaryApplicationSubject } from '@/util/ast/node'
import { expect, test } from 'vitest'

test.each`
  line                               | pattern      | rootExpr   | documentation
  ${'2 + 2'}                         | ${undefined} | ${'2 + 2'} | ${undefined}
  ${'foo = bar'}                     | ${'foo'}     | ${'bar'}   | ${undefined}
  ${'## Documentation\n2 + 2'}       | ${undefined} | ${'2 + 2'} | ${'Documentation'}
  ${'## Documentation\nfoo = 2 + 2'} | ${'foo'}     | ${'2 + 2'} | ${'Documentation'}
`('Node information from AST $line line', ({ line, pattern, rootExpr, documentation }) => {
  const ast = Ast.Ast.parse(line)
  const node = nodeFromAst(ast, false)
  expect(node?.outerExpr).toBe(ast)
  expect(node?.pattern?.code()).toBe(pattern)
  expect(node?.rootExpr.code()).toBe(rootExpr)
  expect(node?.innerExpr.code()).toBe(rootExpr)
  expect(node?.docs?.documentation()).toBe(documentation)
})

test.each(['## Documentation only'])("'%s' should not be a node", (line) => {
  const ast = Ast.Ast.parse(line)
  const node = nodeFromAst(ast, false)
  expect(node).toBeUndefined()
})

test.each([
  { code: 'operator1', expected: undefined },
  { code: 'operator1 foo bar', expected: undefined },
  { code: 'operator1.parse_json', expected: { subject: 'operator1', accesses: ['parse_json'] } },
  {
    code: 'operator1.parse_json operator2.to_json',
    expected: { subject: 'operator1', accesses: ['parse_json'] },
  },
  {
    code: 'operator1.parse_json foo bar',
    expected: { subject: 'operator1', accesses: ['parse_json'] },
  },
  {
    code: 'operator1.parse_json.length',
    expected: { subject: 'operator1', accesses: ['parse_json', 'length'] },
  },
  {
    code: 'operator1.parse_json.length foo bar',
    expected: { subject: 'operator1', accesses: ['parse_json', 'length'] },
  },
  { code: 'operator1 + operator2', expected: undefined },
])('Primary application subject of $code', ({ code, expected }) => {
  const ast = Ast.Ast.parse(code)
  const module = ast.module
  const primaryApplication = primaryApplicationSubject(ast)
  const analyzed = primaryApplication && {
    subject: module.get(primaryApplication.subject).code(),
    accesses: primaryApplication.accessChain.map((id) => {
      const ast = module.get(id)
      assert(ast instanceof Ast.PropertyAccess)
      return ast.rhs.code()
    }),
  }
  expect(analyzed).toEqual(expected)
})
