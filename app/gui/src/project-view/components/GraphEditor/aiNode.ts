/** @file Helpers for AI-prompted User Defined Component nodes. */

import { insertNodeStatements } from '@/composables/nodeCreation'
import { Ast } from '@/util/ast'
import type { Identifier } from '@/util/ast/abstract'
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
