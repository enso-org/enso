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
  /**
   * Renderer-generated UUID identifying this request. Echoed back on every {@link AiProgressEvent}
   * and used as the cancellation key for {@link AiCancelRequest}.
   */
  readonly requestId: string
  readonly prompt: string
  readonly context: AiComponentContext
}

/**
 * Live progress signal dispatched over `Channel.aiProgress`. The renderer routes each event to
 * the placeholder identified by `requestId` and updates its visible status text. The placeholder
 * shows narration text only; `tool.input` is the raw args payload, surfaced in the renderer's
 * web console for debugging.
 */
export type AiProgressEvent =
  | { readonly requestId: string; readonly kind: 'queued' | 'started' }
  | { readonly requestId: string; readonly kind: 'text'; readonly text: string }
  | {
      readonly requestId: string
      readonly kind: 'tool'
      readonly toolName: string
      readonly input: unknown
    }

/** Renderer → main payload for `Channel.cancelAiComponent`. */
export interface AiCancelRequest {
  readonly requestId: string
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
 * byte count of the system prompt plus every user/assistant turn since the last spawn;
 * `durationMs` is the main-process round-trip from stdin write to the terminal `result` envelope.
 */
export interface RequestUsage {
  readonly inputTokens: number
  readonly outputTokens: number
  readonly contextBytes: number
  readonly durationMs: number
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
  /** Per-tool-call correlation id, echoed in {@link AiToolCallReply}. */
  readonly requestId: string
  /**
   * The AI session's `requestId` for the turn this tool call belongs to. The renderer routes the
   * call to the originating placeholder so evaluation runs in the scope captured at enqueue —
   * not in whatever method happens to be visible at tool-call time.
   */
  readonly aiRequestId: string
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
