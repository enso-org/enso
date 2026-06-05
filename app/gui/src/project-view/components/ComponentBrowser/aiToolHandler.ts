/** @file Bridge between the main-process AI MCP server and per-project LS expression evaluation. */
import type { CurrentProjectStore } from '$/components/WithCurrentProject.vue'
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { useOngoingAiPrompts, type OngoingAiPromptsStore } from '@/stores/ongoingAiPrompts'
import type { AiToolCallReply, AiToolCallRequest } from 'enso-common/src/ai'
import { onScopeDispose } from 'vue'

/**
 * Subscribe to mid-turn `evaluateExpression` calls from the AI MCP server, evaluate them against
 * the active project, and reply over `Channel.aiToolReply`. Auto-disposes on scope dispose.
 *
 * The expression is anchored on the AI request's *captured* method body (not the currently-visible
 * one). See `docs/infrastructure/ydoc.md` ("Scope semantics of `nodeExternalId` for `inFrame`
 * one-shots") for why the body's externalId is the right anchor and why graph node ids are not.
 */
export function useAiToolHandler(
  aiPrompts: OngoingAiPromptsStore = useOngoingAiPrompts(),
  currentProject: CurrentProjectStore | undefined = useCurrentProject(true),
): void {
  const electronApi = typeof window === 'undefined' ? undefined : window.api
  if (electronApi == null) return
  let nextCallId = 0
  const dispose = electronApi.ai.onToolCall((request) => {
    const callId = ++nextCallId
    const t0 = performance.now()
    console.log(`[AI tool] called [#${callId}]`, request.expression)
    void handleToolCall(request, aiPrompts, currentProject).then((reply) => {
      const elapsedMs = Math.round(performance.now() - t0)
      console.log(`[AI tool] response [#${callId}, ${elapsedMs}ms]`, reply.result)
      electronApi.ai.replyToolCall(reply)
    })
  })
  onScopeDispose(dispose)
}

async function handleToolCall(
  request: AiToolCallRequest,
  aiPrompts: OngoingAiPromptsStore,
  currentProject: CurrentProjectStore | undefined,
): Promise<AiToolCallReply> {
  const fail = (error: string): AiToolCallReply => ({
    requestId: request.requestId,
    result: { ok: false, error },
  })
  if (request.tool !== 'evaluateExpression') {
    return fail(`unknown tool '${(request as { tool: string }).tool}'`)
  }
  if (currentProject == null) {
    return fail('no active project')
  }
  const entry = aiPrompts.findByRequestId(request.turnRequestId)
  if (entry == null) {
    // The originating AI request is no longer tracked — the store was disposed (project switch),
    // the entry was already cancelled, or the renderer was rebuilt mid-turn.
    return fail('matching AI request is no longer active')
  }
  const projectStore = currentProject.store.value
  try {
    // 25s budget: under the main-process MCP server's 30s timeout so renderer-side errors
    // surface first with an actionable message. On timer expiry the `onTimeout` callback
    // dispatches `executionContext/interrupt` to the LS so the abandoned evaluation does
    // not block the AI's next verification call by occupying the (single-threaded) execution
    // context. The interrupt is context-wide — see project-view/CLAUDE.md for the trade-off.
    const result = await projectStore.queuedExecuteExpressionRaw(
      entry.methodBodyId,
      request.expression,
      25_000,
      () => {
        void projectStore.lsRpcConnection.interruptExecutionContext(
          projectStore.executionContext.id,
        )
      },
    )
    if (result == null) {
      return fail('expression evaluation returned no result')
    }
    if (!result.ok) {
      return fail(translateEngineError(formatLsError(result.error)))
    }
    return { requestId: request.requestId, result: { ok: true, value: result.value } }
  } catch (err) {
    return fail(translateEngineError(formatLsError(err)))
  }
}

function formatLsError(err: unknown): string {
  if (err == null) return 'unknown LS error'
  if (typeof err === 'string') return err
  if (err instanceof Error) return err.message
  if (typeof err === 'object') {
    // Raw `ResultError` instance (or anything with a string `.payload`).
    if ('payload' in err && typeof (err as { payload: unknown }).payload === 'string') {
      return (err as { payload: string }).payload
    }
    // Result-shaped wrapper `{ ok: false, error: ResultError(...) }` — defensive in case a
    // caller ever rejects with an `Err(...)` instead of resolving with one.
    if ('ok' in err && (err as { ok: unknown }).ok === false && 'error' in err) {
      return formatLsError((err as { error: unknown }).error)
    }
  }
  try {
    return JSON.stringify(err)
  } catch {
    return String(err)
  }
}

/**
 * Turn the engine's "Cannot encode class X to byte array." failures (raised when the agent's
 * expression evaluates to a non-Text value) into a hint pointing at `.to_text` / `.to_json` /
 * `.catch_primitive`. Other messages pass through unchanged.
 */
function translateEngineError(message: string): string {
  const match = /^Cannot encode class ([\w$.]+) to byte array\.?$/.exec(message)
  if (match == null) return message
  const className = match[1]!
  const friendlyType =
    className.endsWith('.DataflowError') ? 'a DataflowError'
    : className.endsWith('.PanicException') ? 'a Panic'
    : `a non-Text value of type \`${className}\``
  return (
    `Expression must evaluate to Text, but it evaluated to ${friendlyType}. ` +
    'Wrap with `.to_text`, `.to_display_text`, or `.to_json` to convert. ' +
    'For expressions that may fail, use ' +
    '`((<expr>).catch_primitive (e -> e.to_display_text))` so the result is still Text.'
  )
}
