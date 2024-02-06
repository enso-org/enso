import type { Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Vec2 } from '@/util/data/vec2'

export function nodeFromAst(ast: Ast.Ast): Node | undefined {
  const nodeCode = ast instanceof Ast.Documented ? ast.expression : ast
  if (!nodeCode) return
  return {
    outerExprId: ast.id,
    pattern: nodeCode instanceof Ast.Assignment ? nodeCode.pattern : undefined,
    rootSpan: nodeCode instanceof Ast.Assignment ? nodeCode.expression : nodeCode,
    position: Vec2.Zero,
    vis: undefined,
  }
}

if (import.meta.vitest) {
  const { test, expect } = await import('vitest')
  const { initializeFFI } = await import('shared/ast/ffi')
  await initializeFFI()

  test.each`
    line                               | pattern      | rootSpan
    ${'2 + 2'}                         | ${undefined} | ${'2 + 2'}
    ${'foo = bar'}                     | ${'foo'}     | ${'bar'}
    ${'## Documentation\n2 + 2'}       | ${undefined} | ${'2 + 2'}
    ${'## Documentation\nfoo = 2 + 2'} | ${'foo'}     | ${'2 + 2'}
  `('Node information from AST $line line', ({ line, pattern, rootSpan }) => {
    const ast = Ast.Ast.parse(line)
    const node = nodeFromAst(ast)
    expect(node?.outerExprId).toBe(ast.id)
    expect(node?.pattern?.code()).toBe(pattern)
    expect(node?.rootSpan.code()).toBe(rootSpan)
  })

  test.each(['## Documentation only'])("'%s' should not be a node", (line) => {
    const ast = Ast.Ast.parse(line)
    const node = nodeFromAst(ast)
    expect(node).toBeUndefined()
  })
}
