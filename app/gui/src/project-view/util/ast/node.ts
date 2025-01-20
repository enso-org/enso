import type { NodeDataFromAst } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'
import * as Y from 'yjs'

export const prefixes = Prefixes.FromLines({
  enableRecording:
    'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
})

/** Given a node's outer expression, find the root expression and any statements wrapping it. */
export function nodeRootExpr(ast: Ast.Statement | Ast.Expression): {
  root: Ast.Expression | undefined
  assignment: Ast.Assignment | undefined
} {
  const assignment = ast instanceof Ast.Assignment ? ast : undefined
  const root =
    assignment ? assignment.expression
    : ast instanceof Ast.ExpressionStatement ? ast.expression
    : undefined
  return {
    root,
    assignment,
  }
}

/** Create a Node from the pattern of a function argument. */
export function inputNodeFromAst(ast: Ast.Expression, argIndex: number): NodeDataFromAst {
  return {
    type: 'input',
    outerAst: ast,
    pattern: undefined,
    rootExpr: ast,
    innerExpr: ast,
    prefixes: { enableRecording: undefined },
    primarySubject: undefined,
    conditionalPorts: new Set(),
    argIndex,
  }
}

/** Given a node's outer expression, return all the `Node` fields that depend on its AST structure. */
export function nodeFromAst(ast: Ast.Statement, isOutput: boolean): NodeDataFromAst | undefined {
  const { root, assignment } = nodeRootExpr(ast)
  if (!root) return
  const { innerExpr, matches } = prefixes.extractMatches(root)
  const primaryApplication = primaryApplicationSubject(innerExpr)
  return {
    type: assignment == null && isOutput ? 'output' : 'component',
    outerAst: ast,
    pattern: assignment?.pattern,
    rootExpr: root,
    innerExpr,
    prefixes: matches,
    primarySubject: primaryApplication?.subject,
    conditionalPorts: new Set(primaryApplication?.accessChain ?? []),
    argIndex: undefined,
  }
}

/**
 * Given a node root, find a child AST that is the root of the access chain that is the subject of the primary
 *  application.
 */
export function primaryApplicationSubject(
  ast: Ast.Expression,
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

/** @returns The node's documentation, if this type of node is documentable (currently, this excludes input nodes). */
export function nodeMutableDocumentation(node: NodeDataFromAst): Y.Text | undefined {
  if (!node.outerAst.isStatement()) return
  if (!('mutableDocumentationText' in node.outerAst)) return
  return node.outerAst.mutableDocumentationText()
}

/** @returns The node's documentation text. Returns an empty string if the node has no documentation comment. */
export function nodeDocumentationText(node: NodeDataFromAst): string {
  return nodeMutableDocumentation(node)?.toJSON() ?? ''
}
