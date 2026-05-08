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
 * Per-request usage telemetry from the `claude` session.
 *
 * Cost-side fields (`inputTokens`, `outputTokens`, `cacheReadTokens`, `cacheCreationTokens`)
 * come from the terminal `result` envelope's `usage` — empirically a sum across all
 * underlying completion calls in the turn (multi-hop turns are model → `tool_use` →
 * `tool_result` → model continues …), so these are the right inputs to billing math.
 *
 * `contextTokens` is the **last hop's** prompt size — `input + cache_read + cache_creation`
 * pulled from the `usage` field on the *final* `assistant` envelope before the `result`. That
 * represents the actual context the model saw on the synthesis call, so it grows
 * approximately monotonically across turns and is the right number to compare against the
 * model's context window. If the CLI ever omits per-envelope `usage`, this field falls back
 * to the cost-side sum (and matches the previous semantics).
 *
 * `hopCount` is the number of `assistant` envelopes observed in the turn — i.e. how many
 * times the model produced a response, including any tool-loop intermediates. Useful for
 * sanity-checking variance in the cost-side fields.
 *
 * `contextFromLastHop` is `true` when `contextTokens` came from the final assistant
 * envelope's per-hop `usage`, and `false` when we had to fall back to the cost-side sum
 * (because the CLI didn't surface `message.usage` on the final envelope). On a multi-hop
 * turn, `false` means `contextTokens` overstates actual context occupancy and the value
 * should not be trusted for context-window analysis — see `aiMetrics.appendMetricsRow`,
 * which refuses to write a CSV row when any sample with `hopCount > 0` was a fallback.
 *
 * `durationMs` is the main-process round-trip from stdin write to the terminal `result`
 * envelope.
 */
export interface RequestUsage {
  readonly inputTokens: number
  readonly outputTokens: number
  readonly cacheReadTokens: number
  readonly cacheCreationTokens: number
  readonly contextTokens: number
  readonly contextFromLastHop: boolean
  readonly hopCount: number
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
  readonly requestId: string
  /** The AI session's `requestId` for the turn this tool call belongs to. */
  readonly turnRequestId: string
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
