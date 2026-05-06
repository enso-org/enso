/** @file Shared IPC types for the local Claude agent that generates AI-driven components. */

import { z } from 'zod'
import type { Result } from './utilities/data/result'

/** An identifier visible in the current method's scope, with its inferred type when known. */
export interface AiInScopeBinding {
  readonly identifier: string
  readonly typeName?: string
}

/** Runtime context the renderer attaches to each AI component request. */
export interface AiComponentContext {
  /** Source binding the user dropped into the prompt; absent when generating from scratch. */
  readonly sourceIdentifier?: string
  readonly sourceTypeName?: string
  readonly currentMethodName: string
  readonly currentMethodCode: string
  /** Other bindings in the current method (excludes `sourceIdentifier` when present). */
  readonly inScopeBindings: readonly AiInScopeBinding[]
  /** Verbatim `import` / `from … import …` statements at the top of the module, in source order. */
  readonly moduleImports: readonly string[]
}

/** Payload sent from the renderer to the Electron main process. */
export interface AiComponentRequest {
  readonly prompt: string
  readonly context: AiComponentContext
}

/**
 * Schema for the agent's response — a generated User Defined Component. zod-validated because
 * the payload crosses a trust boundary (decoded from the CLI's stdout). The renderer assembles
 * the call as `Main.<functionName> <callArguments[0]> <callArguments[1]> …`; `argumentNames` are
 * the parameter names referenced inside `body` and are independent of the call-site arguments.
 */
export const aiComponentResponseSchema = z.object({
  functionName: z.string(),
  argumentNames: z.array(z.string()),
  body: z.string(),
  callArguments: z.array(z.string()),
})
export type AiComponentResponse = z.infer<typeof aiComponentResponseSchema>

/**
 * Per-request usage telemetry from the `claude` session. `contextBytes` is the running UTF-8
 * byte count of the system prompt plus every user/assistant turn since the last spawn.
 */
export interface RequestUsage {
  readonly inputTokens: number
  readonly outputTokens: number
  readonly contextBytes: number
}

/** IPC reply shape for `Channel.generateAiComponent`. */
export interface AiComponentIpcReply {
  readonly result: Result<AiComponentResponse>
  readonly usage: RequestUsage | null
}

/**
 * Mid-turn tool call from the main process to the renderer. The renderer evaluates the request
 * against the active project and replies with {@link AiToolCallReply} echoing `requestId`.
 */
export interface AiToolCallRequest {
  readonly requestId: string
  readonly tool: 'evaluateExpression'
  /** Plain Enso expression evaluated in the scope where the AI's new node would land. */
  readonly expression: string
}

/** Reply to {@link AiToolCallRequest}; `value` is the raw text the agent's expression produced. */
export interface AiToolCallReply {
  readonly requestId: string
  readonly result:
    | { readonly ok: true; readonly value: string }
    | { readonly ok: false; readonly error: string }
}
