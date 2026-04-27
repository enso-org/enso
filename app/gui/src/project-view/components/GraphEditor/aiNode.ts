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
  const validatedArgs: Identifier[] = []
  for (const argName of response.argumentNames) {
    const arg = tryIdentifier(argName)
    if (!arg.ok) return Err(`Agent returned an invalid argument name: '${argName}'.`)
    validatedArgs.push(arg.value)
  }

  const callAst = Ast.parseExpression(response.callExpression, edit)
  if (!callAst) {
    return Err(`Agent returned a call expression that did not parse: '${response.callExpression}'.`)
  }
  const validated = validateCallExpression(callAst, baseFunctionName.value, validatedArgs)
  if (!validated.ok) return validated

  // Pick a unique top-level name. If the agent's choice is already taken, suffix it and rewrite
  // the call's function-name token to keep the call-site in sync.
  const uniqueFunctionName = generateUniqueName(baseFunctionName.value, topLevel)
  if (uniqueFunctionName !== baseFunctionName.value) {
    validated.value.functionAccess.setRhs(uniqueFunctionName)
  }

  const functionBody = Ast.parseBlock(response.body.trim(), edit)
  const functionDef = Ast.FunctionDef.new(uniqueFunctionName, validatedArgs, functionBody, {
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
 * Walk the agent-supplied call expression and check it has the shape
 * `Main.<expectedFunctionName> <expectedArgs[0]> <expectedArgs[1]> …`. Returns the inner
 * `PropertyAccess` so the caller can rewrite its RHS if a name collision forces a rename.
 */
function validateCallExpression(
  call: Ast.MutableExpression,
  expectedFunctionName: Identifier,
  expectedArgs: readonly Identifier[],
): Result<{ functionAccess: Ast.MutablePropertyAccess }> {
  const args: Ast.MutableExpression[] = []
  let current: Ast.MutableExpression = call
  while (current instanceof Ast.MutableApp) {
    args.unshift(current.argument)
    current = current.function
  }
  if (!(current instanceof Ast.MutablePropertyAccess)) {
    return Err(
      `Call expression's function should be 'Main.${expectedFunctionName}', got: '${current.code()}'.`,
    )
  }
  const lhs = current.lhs
  if (!(lhs instanceof Ast.Ident) || lhs.code() !== AI_MODULE_NAME) {
    return Err(
      `Call expression's function should be qualified with 'Main', got: '${current.code()}'.`,
    )
  }
  if (current.rhs.code() !== expectedFunctionName) {
    return Err(
      `Call expression names function '${current.rhs.code()}', expected '${expectedFunctionName}'.`,
    )
  }
  if (args.length !== expectedArgs.length) {
    return Err(
      `Call expression has ${args.length} argument(s) but argumentNames lists ${expectedArgs.length}.`,
    )
  }
  for (let i = 0; i < args.length; i++) {
    const arg = args[i]!
    const expected = expectedArgs[i]!
    if (!(arg instanceof Ast.Ident)) {
      return Err(`Call expression argument ${i + 1} should be the identifier '${expected}'.`)
    }
    if (arg.code() !== expected) {
      return Err(`Call expression argument ${i + 1} is '${arg.code()}', expected '${expected}'.`)
    }
  }
  return Ok({ functionAccess: current })
}
