import type { Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'
import { Vec2 } from '@/util/data/vec2'

export const prefixes = Prefixes.FromLines({
  enableRecording:
    'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
})

export function nodeFromAst(ast: Ast.Ast): Node | undefined {
  const { nodeCode, documentation } =
    ast instanceof Ast.Documented
      ? { nodeCode: ast.expression, documentation: ast.documentation() }
      : { nodeCode: ast, documentation: undefined }
  if (!nodeCode) return
  const pattern = nodeCode instanceof Ast.Assignment ? nodeCode.pattern : undefined
  const rootExpr = nodeCode instanceof Ast.Assignment ? nodeCode.expression : nodeCode
  const { innerExpr, matches } = prefixes.extractMatches(rootExpr)
  return {
    outerExprId: ast.id,
    pattern,
    rootExpr,
    innerExpr,
    position: Vec2.Zero,
    vis: undefined,
    prefixes: matches,
    primarySubject: primaryApplicationSubject(innerExpr),
    documentation,
  }
}

/** Given a node root, find a child AST that is the root of the access chain that is the subject of the primary
 *  application.
 */
export function primaryApplicationSubject(ast: Ast.Ast): Ast.AstId | undefined {
  // Descend into LHS of any sequence of applications.
  while (ast instanceof Ast.App) ast = ast.function
  // Require a sequence of at least one property access; descend into LHS.
  if (!(ast instanceof Ast.PropertyAccess)) return
  while (ast instanceof Ast.PropertyAccess && ast.lhs) ast = ast.lhs
  // The leftmost element must be an identifier.
  if (!(ast instanceof Ast.Ident)) return
  return ast.id
}

if (import.meta.vitest) {
  const { test, expect } = await import('vitest')
  const { initializeFFI } = await import('shared/ast/ffi')
  await initializeFFI()

  test.each`
    line                               | pattern      | rootExpr   | documentation
    ${'2 + 2'}                         | ${undefined} | ${'2 + 2'} | ${undefined}
    ${'foo = bar'}                     | ${'foo'}     | ${'bar'}   | ${undefined}
    ${'## Documentation\n2 + 2'}       | ${undefined} | ${'2 + 2'} | ${'Documentation'}
    ${'## Documentation\nfoo = 2 + 2'} | ${'foo'}     | ${'2 + 2'} | ${'Documentation'}
  `('Node information from AST $line line', ({ line, pattern, rootExpr, documentation }) => {
    const ast = Ast.Ast.parse(line)
    const node = nodeFromAst(ast)
    expect(node?.outerExprId).toBe(ast.id)
    expect(node?.pattern?.code()).toBe(pattern)
    expect(node?.rootExpr.code()).toBe(rootExpr)
    expect(node?.innerExpr.code()).toBe(rootExpr)
    expect(node?.documentation).toBe(documentation)
  })

  test.each(['## Documentation only'])("'%s' should not be a node", (line) => {
    const ast = Ast.Ast.parse(line)
    const node = nodeFromAst(ast)
    expect(node).toBeUndefined()
  })
}
