import { Ast } from '@/util/ast'
import { nodeFromAst } from '@/util/ast/node'
import { initializeFFI } from 'shared/ast/ffi'
import { expect, test } from 'vitest'

await initializeFFI()

test.each`
  line                               | pattern      | rootSpan   | documentation
  ${'2 + 2'}                         | ${undefined} | ${'2 + 2'} | ${undefined}
  ${'foo = bar'}                     | ${'foo'}     | ${'bar'}   | ${undefined}
  ${'## Documentation\n2 + 2'}       | ${undefined} | ${'2 + 2'} | ${'Documentation'}
  ${'## Documentation\nfoo = 2 + 2'} | ${'foo'}     | ${'2 + 2'} | ${'Documentation'}
`('Node information from AST $line line', ({ line, pattern, rootSpan, documentation }) => {
  const ast = Ast.Ast.parse(line)
  const node = nodeFromAst(ast)
  expect(node?.outerExprId).toBe(ast.id)
  expect(node?.pattern?.code()).toBe(pattern)
  expect(node?.rootSpan.code()).toBe(rootSpan)
  expect(node?.documentation).toBe(documentation)
})

test.each(['## Documentation only'])("'%s' should not be a node", (line) => {
  const ast = Ast.Ast.parse(line)
  const node = nodeFromAst(ast)
  expect(node).toBeUndefined()
})
