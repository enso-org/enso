import type { NodeDataFromAst } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'

export const prefixes = Prefixes.FromLines({
  enableRecording:
    'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
})

/** Given a node's outer expression, find the root expression and any statements wrapping it. */
export function nodeRootExpr(ast: Ast.Ast): {
  root: Ast.Ast | undefined
  docs: Ast.Documented | undefined
  assignment: Ast.Assignment | undefined
} {
  const [withinDocs, docs] =
    ast instanceof Ast.Documented ? [ast.expression, ast] : [ast, undefined]
  const [withinAssignment, assignment] =
    withinDocs instanceof Ast.Assignment ?
      [withinDocs.expression, withinDocs]
    : [withinDocs, undefined]
  return {
    root: withinAssignment,
    docs,
    assignment,
  }
}

/** TODO: Add docs */
export function inputNodeFromAst(ast: Ast.Ast, argIndex: number): NodeDataFromAst {
  return {
    type: 'input',
    outerExpr: ast,
    pattern: undefined,
    rootExpr: ast,
    innerExpr: ast,
    prefixes: { enableRecording: undefined },
    primarySubject: undefined,
    conditionalPorts: new Set(),
    docs: undefined,
    argIndex,
  }
}

/** Given a node's outer expression, return all the `Node` fields that depend on its AST structure. */
export function nodeFromAst(ast: Ast.Ast, isOutput: boolean): NodeDataFromAst | undefined {
  const { root, docs, assignment } = nodeRootExpr(ast)
  if (!root) return
  const { innerExpr, matches } = prefixes.extractMatches(root)
  const type = assignment == null && isOutput ? 'output' : 'component'
  const primaryApplication = primaryApplicationSubject(innerExpr)
  return {
    type,
    outerExpr: ast,
    pattern: assignment?.pattern,
    rootExpr: root,
    innerExpr,
    prefixes: matches,
    primarySubject: primaryApplication?.subject,
    conditionalPorts: new Set(primaryApplication?.accessChain ?? []),
    docs,
    argIndex: undefined,
  }
}

/**
 * Given a node root, find a child AST that is the root of the access chain that is the subject of the primary
 *  application.
 */
export function primaryApplicationSubject(
  ast: Ast.Ast,
): { subject: Ast.AstId; accessChain: Ast.AstId[] } | undefined {
  // Descend into LHS of any sequence of applications.
  while (ast instanceof Ast.App) ast = ast.function
  const { subject, accessChain } = Ast.accessChain(ast)
  // Require at least one property access.
  if (accessChain.length === 0) return
  // The leftmost element must be an identifier or a placeholder.
  if (
    !(
      (subject instanceof Ast.Ident && !subject.isTypeOrConstructor()) ||
      subject instanceof Ast.Wildcard
    )
  )
    return
  return { subject: subject.id, accessChain: accessChain.map((ast) => ast.id) }
}
