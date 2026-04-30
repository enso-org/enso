/** @file Helpers for AI-prompted User Defined Component nodes. */

import { insertNodeStatements } from '@/composables/nodeCreation'
import { Ast } from '@/util/ast'
import type { Identifier } from '@/util/ast/abstract'
import type { Vec2 } from '@/util/data/vec2'
import type { Icon } from '@/util/iconMetadata/iconName'
import { frontmatter } from '../ComponentHelp/metadata'
import { generateUniqueName } from './widgets/WidgetFunctionDef/argumentAst'

/** Marker that identifies a node's documentation comment as an AI-generated prompt. */
export const AI_COMMENT_PREFIX = 'AI: '

const AI_MODULE_NAME = 'Main' as Identifier
const AI_FUNCTION_NAME_PREFIX = 'ai_component' as Identifier
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
  body: string
  sourceIdentifier: Identifier
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
 * whose body is the supplied Enso source, plus an `Assignment` in the current method that
 * calls it with the source identifier. The assignment carries a documentation comment of
 * the form `AI: <prompt>`, which the graph editor renders as the node's prompt.
 */
export function createAiNode(options: CreateAiNodeOptions) {
  const { edit, topLevel, currentMethodName, binding, position, payload } = options
  const { prompt, body, sourceIdentifier } = payload
  const found = Ast.findModuleMethod(topLevel, currentMethodName)
  if (!found) return
  const { statement: currentMethod, index: currentMethodLine } = found

  const functionName = generateUniqueName(AI_FUNCTION_NAME_PREFIX, topLevel)
  const functionBody = Ast.parseBlock(body.trim(), edit)
  const functionDef = Ast.FunctionDef.new(functionName, [sourceIdentifier], functionBody, {
    edit,
    documentation: frontmatter({ icon: AI_ICON }) + AI_FUNCTION_DOC_PLACEHOLDER,
  })

  const call = Ast.App.PositionalSequence(
    Ast.PropertyAccess.new(edit, Ast.Ident.new(edit, AI_MODULE_NAME), functionName),
    [Ast.Ident.new(edit, sourceIdentifier)],
  )
  call.setNodeMetadata({ position: position.xy() })
  const assignment = Ast.Assignment.new(binding, call, {
    edit,
    documentation: AI_COMMENT_PREFIX + prompt,
  })

  insertNodeStatements(currentMethod.bodyAsBlock(), [assignment])
  topLevel.insert(currentMethodLine, functionDef, undefined)
}
