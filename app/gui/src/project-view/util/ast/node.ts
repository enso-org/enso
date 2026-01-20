import type { NodeDataFromAst } from '$/providers/openedProjects/graph'
import {
  emptyPrimaryApplication,
  type PrimaryApplication,
} from '$/providers/openedProjects/graph/graphDatabase'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'
import { computed } from 'vue'
import * as Y from 'yjs'

// Computed used here intentionally to delay initialization until first use. Otherwise we get issues
// related to module load order or calling wasm parser too early.
export const prefixes = computed(() =>
  Prefixes.FromLines({
    enableRecording:
      'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
  }),
)

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
    pattern: ast,
    rootExpr: ast,
    innerExpr: ast,
    prefixes: { enableRecording: undefined },
    primaryApplication: { function: null, accessChain: null, selfArgument: null },
    conditionalPorts: new Set(),
    argIndex,
  }
}

/** Given a node's outer expression, return all the `Node` fields that depend on its AST structure. */
export function nodeFromAst(ast: Ast.Statement, isOutput: boolean): NodeDataFromAst | undefined {
  const { root, assignment } = nodeRootExpr(ast)
  if (!root) return
  const { innerExpr, matches } = prefixes.value.extractMatches(root)
  const primaryApp = primaryApplication(innerExpr)
  return {
    type: assignment == null && isOutput ? 'output' : 'component',
    outerAst: ast,
    pattern: assignment?.pattern,
    rootExpr: root,
    innerExpr,
    prefixes: matches,
    primaryApplication: primaryApp,
    conditionalPorts: new Set(primaryApp?.accessChain ?? []),
    argIndex: undefined,
  }
}

/**
 * Given a node root, find the primary application of the node, if any.
 * Returns empty primary application information otherwise.
 */
export function primaryApplication(ast: Ast.Expression): PrimaryApplication {
  // Descend into LHS of any sequence of applications.
  while (ast instanceof Ast.App) ast = ast.function
  const unrolledChain = Ast.accessChain(ast)
  let subject = unrolledChain.subject
  const accessChain = unrolledChain.accessChain
  // Require at least one property access.
  if (accessChain.length === 0) return emptyPrimaryApplication()

  const isAcceptableSubject = (subject: Ast.Ast) =>
    (subject instanceof Ast.Ident && !subject.isTypeOrConstructor()) ||
    subject instanceof Ast.Wildcard

  // Descend into any sequence of groups or type annotations.
  while (subject instanceof Ast.Group || subject instanceof Ast.TypeAnnotated) {
    if (subject instanceof Ast.Group && subject.expression) {
      subject = subject.expression
    } else if (subject instanceof Ast.TypeAnnotated && subject.expression) {
      subject = subject.expression
    } else {
      break
    }
  }
  if (!isAcceptableSubject(subject)) return emptyPrimaryApplication()
  return {
    selfArgument: subject.id,
    function: ast.id,
    accessChain: accessChain.map((ast) => ast.id),
  }
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
