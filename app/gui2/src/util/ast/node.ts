import type { Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Vec2 } from '@/util/data/vec2'

export function nodeFromAst(ast: Ast.Ast): Node {
  if (ast instanceof Ast.Assignment) {
    return {
      outerExpr: ast,
      pattern: ast.pattern ?? undefined,
      rootSpan: ast.expression ?? ast,
      position: Vec2.Zero,
      vis: undefined,
    }
  } else {
    return {
      outerExpr: ast,
      pattern: undefined,
      rootSpan: ast,
      position: Vec2.Zero,
      vis: undefined,
    }
  }
}
