import type { NodeDataFromAst } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'

export let prefixes!: ReturnType<typeof makePrefixes>

function makePrefixes() {
  return Prefixes.FromLines({
    enableRecording:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
  })
}

/** MUST be called after `initializeFFI`. */
export function initializePrefixes() {
  prefixes = makePrefixes()
}

export function nodeFromAst(ast: Ast.Ast): NodeDataFromAst | undefined {
  const { nodeCode, documentation } =
    ast instanceof Ast.Documented ?
      { nodeCode: ast.expression, documentation: ast.documentation() }
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
