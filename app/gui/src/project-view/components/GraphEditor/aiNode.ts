/** @file Helpers for AI-prompted User Defined Component nodes. */

import { insertNodeStatements } from '@/composables/nodeCreation'
import { Ast } from '@/util/ast'
import { deleteFromParentBlock, type Identifier } from '@/util/ast/abstract'
import type { Vec2 } from '@/util/data/vec2'
import type { Icon } from '@/util/iconMetadata/iconName'
import { tryIdentifier } from '@/util/qualifiedName'
import type { AiComponentResponse } from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { frontmatter } from '../ComponentHelp/metadata'
import { generateUniqueName } from './widgets/WidgetFunctionDef/argumentAst'

/** Marker that identifies a node's documentation comment as an AI-generated prompt. */
export const AI_COMMENT_PREFIX = 'AI: '

const AI_MODULE_NAME = 'Main' as Identifier
const AI_ICON: Icon = 'robot'
const AI_FUNCTION_DOC_PLACEHOLDER = 'Documentation can be added here.'

/** Extract the prompt text from a documentation string, or `null` if it is not an AI comment. */
export function readAiPrompt(documentation: string | undefined): string | null {
  if (documentation == null || !documentation.startsWith(AI_COMMENT_PREFIX)) return null
  return documentation.slice(AI_COMMENT_PREFIX.length)
}

/** `true` when the given statement is an assignment whose documentation is an AI prompt. */
export function isAiAssignment(ast: Ast.Ast): boolean {
  return (
    ast instanceof Ast.Assignment &&
    ast.mutableDocumentationText().toJSON().startsWith(AI_COMMENT_PREFIX)
  )
}

/** Data produced by the ComponentBrowser when an AI prompt is accepted. */
export interface AcceptedAiPayload {
  prompt: string
  response: AiComponentResponse
}

export interface CreateAiNodeOptions {
  edit: Ast.MutableModule
  topLevel: Ast.MutableBodyBlock
  currentMethodName: string
  binding: Identifier
  position: Vec2
  payload: AcceptedAiPayload
}

/**
 * Insert a User Defined Component driven by an AI prompt: a new top-level `FunctionDef`
 * whose signature and body are supplied by the agent, plus an `Assignment` in the current
 * method that calls it. The assignment carries a documentation comment of the form
 * `AI: <prompt>`, which the graph editor renders as the node's prompt.
 *
 * Returns an `Err` when the agent's response can't be turned into a valid AST — the caller
 * should surface that to the user as a toast and abort the surrounding `module.edit` so the
 * partial work is rolled back.
 */
export function createAiNode(options: CreateAiNodeOptions): Result {
  const { edit, topLevel, currentMethodName, binding, position, payload } = options
  const { prompt, response } = payload
  const found = Ast.findModuleMethod(topLevel, currentMethodName)
  if (!found) return Err(`Cannot find current method '${currentMethodName}' in the module.`)
  const { statement: currentMethod, index: currentMethodLine } = found

  const baseFunctionName = tryIdentifier(response.functionName)
  if (!baseFunctionName.ok) {
    return Err(`Agent returned an invalid function name: '${response.functionName}'.`)
  }
  const parameterNames: Identifier[] = []
  for (const argName of response.argumentNames) {
    const arg = tryIdentifier(argName)
    if (!arg.ok) return Err(`Agent returned an invalid argument name: '${argName}'.`)
    parameterNames.push(arg.value)
  }
  if (response.callArguments.length !== parameterNames.length) {
    return Err(
      `Agent returned ${response.callArguments.length} call argument(s) but the function takes ${parameterNames.length}.`,
    )
  }
  const callArgAsts: Ast.Owned<Ast.MutableExpression>[] = []
  for (const [i, argSource] of response.callArguments.entries()) {
    const argAst = Ast.parseExpression(argSource, edit)
    if (!argAst) {
      return Err(`Agent returned a call argument that did not parse (#${i + 1}): '${argSource}'.`)
    }
    callArgAsts.push(argAst)
  }

  const uniqueFunctionName = generateUniqueName(baseFunctionName.value, topLevel)
  const callAst = Ast.App.PositionalSequence(
    Ast.PropertyAccess.new(edit, Ast.Ident.new(edit, AI_MODULE_NAME), uniqueFunctionName),
    callArgAsts,
  )

  const functionBody = Ast.parseBlock(response.body.trim(), edit)
  const functionDef = Ast.FunctionDef.new(uniqueFunctionName, parameterNames, functionBody, {
    edit,
    documentation: frontmatter({ icon: AI_ICON }) + AI_FUNCTION_DOC_PLACEHOLDER,
  })

  callAst.setNodeMetadata({ position: position.xy() })
  const assignment = Ast.Assignment.new(binding, callAst, {
    edit,
    documentation: AI_COMMENT_PREFIX + prompt,
  })

  insertNodeStatements(currentMethod.bodyAsBlock(), [assignment])
  topLevel.insert(currentMethodLine, functionDef, undefined)
  return Ok()
}

/**
 * Recover the existing top-level FunctionDef referenced by an AI Assignment's call site, plus
 * its full source code. Returns `null` if the assignment's call site does not match the AI shape
 * (`Main.<functionName> <args…>`) or the FunctionDef has been removed from the module — in the
 * latter case {@link updateAiNode} falls back to inserting a fresh FunctionDef before the
 * assignment's enclosing method.
 */
export function readAiCallTarget(
  assignment: Ast.Assignment,
  topLevel: Ast.BodyBlock,
): {
  functionName: Identifier
  functionDef: Ast.FunctionDef
  definitionCode: string
  /**
   * Index of the FunctionDef within `topLevel.lines` — captured so an edit can replace the
   * definition at the same line rather than appending below `main`.
   */
  topLevelIndex: number
} | null {
  // The call is shaped `Main.<functionName> a b c`. Walk through `Ast.App` to find the
  // innermost function expression, which should be the `Main.<functionName>` property access.
  let fn: Ast.Expression = assignment.expression
  while (fn instanceof Ast.App) fn = fn.function
  if (!(fn instanceof Ast.PropertyAccess)) return null
  const candidate = fn.rhs.code()
  const asIdent = tryIdentifier(candidate)
  if (!asIdent.ok) return null
  const found = Ast.findModuleMethod(topLevel, asIdent.value)
  if (!found) return null
  return {
    functionName: asIdent.value,
    functionDef: found.statement,
    definitionCode: found.statement.code(),
    topLevelIndex: found.index,
  }
}

export interface UpdateAiNodeOptions {
  edit: Ast.MutableModule
  topLevel: Ast.MutableBodyBlock
  /** The Assignment node identifying the AI-generated call site to rewrite. */
  assignment: Ast.MutableAssignment
  /**
   * The name of the method that owns the assignment. Used as the FunctionDef insertion point
   * when the previously-generated FunctionDef has been removed from the module.
   */
  currentMethodName: string
  prompt: string
  response: AiComponentResponse
}

/**
 * Rewrite an existing AI-generated node in place. The agent's reply may change the function
 * name, parameter list, body, and call arguments; only the call-site binding identifier (the
 * `Assignment.pattern`) and the call expression's metadata (position, visualization,
 * colorOverride, displayMode, and per-widget state) are preserved. The existing top-level
 * FunctionDef is removed and replaced with a freshly built one at the same line; the call AST
 * under the Assignment is replaced. If the previous FunctionDef is no longer in the module
 * (e.g. the user deleted it manually) the new one is inserted before the enclosing method.
 *
 * Returns `Err` when the agent's response can't be turned into a valid AST or the enclosing
 * method can't be located for the fallback insertion.
 */
export function updateAiNode(options: UpdateAiNodeOptions): Result {
  const { edit, topLevel, assignment, currentMethodName, prompt, response } = options
  const baseFunctionName = tryIdentifier(response.functionName)
  if (!baseFunctionName.ok) {
    return Err(`Agent returned an invalid function name: '${response.functionName}'.`)
  }
  const parameterNames: Identifier[] = []
  for (const argName of response.argumentNames) {
    const arg = tryIdentifier(argName)
    if (!arg.ok) return Err(`Agent returned an invalid argument name: '${argName}'.`)
    parameterNames.push(arg.value)
  }
  if (response.callArguments.length !== parameterNames.length) {
    return Err(
      `Agent returned ${response.callArguments.length} call argument(s) but the function takes ${parameterNames.length}.`,
    )
  }
  const callArgAsts: Ast.Owned<Ast.MutableExpression>[] = []
  for (const [i, argSource] of response.callArguments.entries()) {
    const argAst = Ast.parseExpression(argSource, edit)
    if (!argAst) {
      return Err(`Agent returned a call argument that did not parse (#${i + 1}): '${argSource}'.`)
    }
    callArgAsts.push(argAst)
  }

  // Either replace the existing FunctionDef at its line, or fall back to inserting before the
  // enclosing method.
  const existing = readAiCallTarget(assignment, topLevel)
  let topLevelIndex: number
  if (existing != null) {
    // Remove the existing FunctionDef first so its name doesn't count as a collision when we
    // pick a unique name for the new one. This lets the agent keep the same name across edits.
    topLevelIndex = existing.topLevelIndex
    const mutableExisting = edit.get(existing.functionDef.id)
    if (mutableExisting instanceof Ast.MutableFunctionDef) {
      deleteFromParentBlock(mutableExisting)
    }
  } else {
    const fallbackMethod = Ast.findModuleMethod(topLevel, currentMethodName)
    if (!fallbackMethod) {
      return Err(`Cannot find current method '${currentMethodName}' in the module.`)
    }
    topLevelIndex = fallbackMethod.index
  }

  const uniqueFunctionName = generateUniqueName(baseFunctionName.value, topLevel)
  const newCallAst = Ast.App.PositionalSequence(
    Ast.PropertyAccess.new(edit, Ast.Ident.new(edit, AI_MODULE_NAME), uniqueFunctionName),
    callArgAsts,
  )

  // Preserve every metadata field from the old expression. `setExpression` would otherwise
  // attach `newCallAst` with its empty metadata, losing the user's visualization choice,
  // color override, expand/collapse state, and any per-widget configuration.
  const oldCallExpr = assignment.expression
  const oldNodeMeta = oldCallExpr.nodeMetadata
  newCallAst.setNodeMetadata({
    position: oldNodeMeta.get('position'),
    visualization: oldNodeMeta.get('visualization'),
    colorOverride: oldNodeMeta.get('colorOverride'),
    displayMode: oldNodeMeta.get('displayMode'),
  })
  for (const [widgetKey, widgetMeta] of oldCallExpr.widgetsMetadata()) {
    newCallAst.setWidgetMetadata(widgetKey, widgetMeta)
  }

  const functionBody = Ast.parseBlock(response.body.trim(), edit)
  const functionDef = Ast.FunctionDef.new(uniqueFunctionName, parameterNames, functionBody, {
    edit,
    documentation: frontmatter({ icon: AI_ICON }) + AI_FUNCTION_DOC_PLACEHOLDER,
  })

  assignment.setExpression(newCallAst)
  // Replace the documentation text in-place via the Y.Text accessor.
  const docs = assignment.mutableDocumentationText()
  const newDocs = AI_COMMENT_PREFIX + prompt
  if (docs.toJSON() !== newDocs) {
    docs.delete(0, docs.length)
    docs.insert(0, newDocs)
  }

  topLevel.insert(topLevelIndex, functionDef, undefined)
  return Ok()
}
