/**
 * @file Electron-main-side adapter that runs the local Claude Agent SDK to generate
 * the body of a User Defined Component from a natural-language prompt. Exposed to the
 * renderer via IPC; the renderer never imports the SDK directly.
 */
import { query } from '@anthropic-ai/claude-agent-sdk'
import { ipcMain } from 'electron'
import type { AiComponentRequest, AiComponentResponse } from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { Channel } from './ipc.js'

const REQUEST_TIMEOUT_MS = 60_000
const MAX_AGENT_TURNS = 2

// =====================
// === System prompt ===
// =====================

const ENSO_CHEAT_SHEET = `\
Enso is a functional, indentation-sensitive language for data work.
Syntax essentials:
- Assignment: \`name = expression\`. The last expression in a block is its value.
- Method chain: \`table.filter predicate . sort ["date"]\`.
- Lambda: \`x -> x + 1\`.
- Text literals use double quotes: "hello".
- Comments start with \`#\` and are rare in generated code.
- Blocks: indented lines belonging to the same statement.
- Qualified names: \`Standard.Base.Data.Vector.Vector.new\`.
Common stdlib entry points (Standard.Base / Standard.Table):
- \`Vector.new count fn\`, \`Vector.filter\`, \`Vector.map\`, \`Vector.reduce\`.
- \`Table.filter\`, \`Table.select_columns\`, \`Table.sort\`, \`Table.aggregate\`.
- \`Text.contains\`, \`Text.starts_with\`, \`Text.split\`.
- \`Data.read path\`, \`Data.write path value\`.`

const SYSTEM_PROMPT = `\
You generate the body of a User Defined Component in Enso — a small block of Enso code that takes one input binding and produces one output value.

${ENSO_CHEAT_SHEET}

You will receive:
- An input binding identifier (the parameter the block should operate on).
- Its Enso type name, when known.
- A natural-language description of what the block should do.

You must return a JSON object matching the supplied schema:
- \`body\`: a string containing the Enso block. Every line belongs to the block; no leading or trailing blank lines. The final line's expression is the value the block returns. Do NOT include the function signature, the \`=\` sign, or any \`main\` / module wrapper — only the body lines.

Rules:
- Reference the input binding by its identifier; do not invent another name for it.
- Prefer simple, readable code; break long pipelines over multiple lines using intermediate bindings.
- Return only valid Enso — avoid placeholders, pseudocode, or commentary.`

const RESPONSE_SCHEMA = {
  type: 'object',
  properties: {
    body: { type: 'string' },
  },
  required: ['body'],
  additionalProperties: false,
} as const

// =================
// === Prompt IO ===
// =================

function buildUserPrompt(request: AiComponentRequest): string {
  const { prompt, context } = request
  const typeLine =
    context.sourceTypeName ?
      `Input binding type: ${context.sourceTypeName}`
    : 'Input binding type: unknown'
  return `Input binding identifier: ${context.sourceIdentifier}
${typeLine}

User request: ${prompt}`
}

function extractBody(structuredOutput: unknown): string | null {
  if (
    structuredOutput != null &&
    typeof structuredOutput === 'object' &&
    'body' in structuredOutput &&
    typeof structuredOutput.body === 'string'
  ) {
    return structuredOutput.body
  }
  return null
}

// ====================
// === Public entry ===
// ====================

/** Run the local Claude agent to produce a User Defined Component body. */
export async function generateAiComponent(
  request: AiComponentRequest,
): Promise<Result<AiComponentResponse>> {
  if (!process.env.ANTHROPIC_API_KEY) {
    return Err('ANTHROPIC_API_KEY is not set in the Electron main-process environment')
  }
  const abortController = new AbortController()
  const timeout = setTimeout(() => abortController.abort(), REQUEST_TIMEOUT_MS)
  try {
    for await (const message of query({
      prompt: buildUserPrompt(request),
      options: {
        systemPrompt: SYSTEM_PROMPT,
        allowedTools: [],
        outputFormat: { type: 'json_schema', schema: RESPONSE_SCHEMA },
        maxTurns: MAX_AGENT_TURNS,
        abortController,
      },
    })) {
      if (message.type === 'result') {
        if (message.subtype === 'success') {
          const body = extractBody(message.structured_output)
          if (body == null) {
            return Err('Claude returned a result without a valid `body` field')
          }
          return Ok({ body })
        }
        const detail = message.errors.length > 0 ? `: ${message.errors.join('; ')}` : ''
        return Err(`Claude agent ended with ${message.subtype}${detail}`)
      }
    }
    return Err('Claude agent query ended without a result message')
  } catch (error) {
    if (abortController.signal.aborted) {
      return Err(`Claude agent timed out after ${REQUEST_TIMEOUT_MS}ms`)
    }
    const detail = error instanceof Error ? error.message : String(error)
    return Err(`Claude agent query failed: ${detail}`)
  } finally {
    clearTimeout(timeout)
  }
}

// ===================
// === IPC binding ===
// ===================

/** Register the {@link Channel.generateAiComponent} IPC handler. */
export function initClaudeAgentIpc() {
  ipcMain.handle(Channel.generateAiComponent, async (_event, request: AiComponentRequest) =>
    generateAiComponent(request),
  )
}
