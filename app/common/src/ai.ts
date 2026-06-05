/** @file Shared IPC types for the local Claude agent that generates AI-driven components. */

import { z } from 'zod'
import type { Result } from './utilities/data/result'

/** An identifier visible in the current method's scope, with its inferred type when known. */
export interface AiInScopeBinding {
  readonly identifier: string
  readonly typeName?: string
}

/**
 * Captured when the user re-opens the Component Browser on an existing AI-generated node and
 * commits an updated prompt. The agent uses this to rewrite the existing User Defined Component
 * rather than generating a fresh one. The agent is free to change the function name, parameter
 * list, body, and call arguments; only the call-site binding identifier on the user's graph is
 * preserved across the edit.
 */
export interface AiEditContext {
  /** The previous natural-language prompt that produced the current node. */
  readonly previousPrompt: string
  /**
   * The previous top-level `FunctionDef` source — signature, parameters, and body.
   * Absent when the previous definition could not be recovered (e.g. the user manually deleted
   * the top-level function), in which case the rewrite degrades to a fresh generation.
   */
  readonly previousDefinition?: string
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
  /** When set, the request is an edit of an existing AI node. See {@link AiEditContext}. */
  readonly editContext?: AiEditContext
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

/** Per-request usage telemetry from the `claude` session. */
export interface RequestUsage {
  /**
   * Prompt tokens billed for this turn. Sourced from the terminal `result` envelope's `usage`,
   * which the CLI sums across all underlying completion calls on a multi-hop turn (model →
   * `tool_use` → `tool_result` → model continues …). The right input for billing math, but
   * not comparable to the model's context-window size — use `contextTokens` for that.
   */
  readonly inputTokens: number
  /** Completion tokens billed for this turn. Sourced like {@link inputTokens}. */
  readonly outputTokens: number
  /** Cache-read tokens billed for this turn. Sourced like {@link inputTokens}. */
  readonly cacheReadTokens: number
  /** Cache-creation tokens billed for this turn. Sourced like {@link inputTokens}. */
  readonly cacheCreationTokens: number
  /**
   * Size of the prompt on the **last hop** of the turn — `input + cache_read + cache_creation`
   * pulled from the `usage` field on the *final* `assistant` envelope before the `result`.
   * Represents the actual context the model saw on the synthesis call, so it grows
   * approximately monotonically across turns and is the right number to compare against the
   * model's context window. Falls back to the cost-side sum when the CLI omits per-envelope
   * `usage`; see {@link contextFromLastHop} to distinguish the two cases.
   */
  readonly contextTokens: number
  /**
   * `true` when {@link contextTokens} came from the final assistant envelope's per-hop `usage`,
   * `false` when it fell back to the cost-side sum (because the CLI didn't surface
   * `message.usage` on the final envelope). On a multi-hop turn, `false` means
   * `contextTokens` overstates actual context occupancy and the value should not be trusted
   * for context-window analysis — see `aiMetrics.appendMetricsRow`, which flags such samples
   * by suffixing the metrics-row `status` column with ` (broken)`.
   */
  readonly contextFromLastHop: boolean
  /**
   * Number of `assistant` envelopes observed in the turn — i.e. how many times the model
   * produced a response, including any tool-loop intermediates. Useful for sanity-checking
   * variance in the cost-side fields.
   */
  readonly hopCount: number
  /** Main-process round-trip from stdin write to the terminal `result` envelope. */
  readonly durationMs: number
  /**
   * `true` when this turn ran on a `ChildAgent` that was promoted from "warming" to "primary"
   * just before this turn started — i.e. the conversation history was reset by a context-rotation
   * (see `claudeAgent.ts`'s threshold-rotation policy). `undefined`/`false` for normal turns.
   */
  readonly freshAgent?: boolean
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
