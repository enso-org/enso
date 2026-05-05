import type { CurrentProjectStore } from '$/components/WithCurrentProject.vue'
import { useCurrentProject, useGraphStore } from '$/components/WithCurrentProject.vue'
import type { GraphStore } from '$/providers/openedProjects/graph'
import type { AiToolCallReply, AiToolCallRequest } from 'enso-common/src/ai'
import { onScopeDispose } from 'vue'

/**
 * Subscribe to mid-turn tool calls dispatched by the in-process MCP server. This composable is
 * mounted inside `WithCurrentProject`'s component tree alongside `useAI()`, so when the project
 * view unmounts (project close, navigation back to dashboard) the listener is automatically
 * disposed and any subsequent tool call from a leftover `claude` turn answers `no active project`
 * as a fallback once the renderer-side handler is gone.
 *
 * Tool dispatch contract: the main-process MCP server sends `Channel.aiToolCall` with a unique
 * `requestId`; we evaluate the request against the active project's LS connection via
 * `queuedExecuteExpression` (the queued variant — it cooperates with the existing concurrency
 * cap and retry/backoff in `project.ts`), then reply with `Channel.aiToolReply` echoing the
 * `requestId`. Failure to reply triggers the server's 30 s per-call timeout, which surfaces back
 * to the model as a structured error.
 */
export function useAiToolHandler(
  graphStore: GraphStore = useGraphStore(),
  currentProject: CurrentProjectStore | undefined = useCurrentProject(true),
): void {
  const electronApi = typeof window === 'undefined' ? undefined : window.api
  if (electronApi == null) return
  const dispose = electronApi.ai.onToolCall((request) => {
    console.debug('Tool called', request.expression)
    void handleToolCall(request, graphStore, currentProject).then((reply) => {
      console.debug('Tool response', reply.result)
      electronApi.ai.replyToolCall(reply)
    })
  })
  onScopeDispose(dispose)
}

async function handleToolCall(
  request: AiToolCallRequest,
  graphStore: GraphStore,
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
  const projectStore = currentProject.store.value
  // The LS uses `expressionId` to determine which method scope the new expression evaluates in.
  // Anchor on the last in-scope binding in the current method: every binding defined earlier in
  // the method (including the anchor itself) is visible — exactly the scope where the AI's
  // generated `body` would land. Iteration order of `nodeOutputPorts.allForward()` matches the
  // method's textual order.
  const anchor = lastInScopeNodeId(graphStore)
  if (anchor == null) {
    return fail('no in-scope binding to anchor scope')
  }
  try {
    // Raw text path: the agent picks the encoding (`.to_text`, `.to_json`, etc.), so we forward
    // bytes through unchanged. Wrapping the expression on our side would mask the agent's choice
    // and force a stringification cost on simple value previews. The 25s budget aligns with the
    // main-process MCP server's 30s per-call timeout — leave a small margin so the renderer's
    // failure surfaces first with an actionable message instead of the bare MCP timeout.
    const result = await projectStore.queuedExecuteExpressionRaw(anchor, request.expression, 25_000)
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

function lastInScopeNodeId(
  graphStore: GraphStore,
): import('ydoc-shared/yjsModel').ExternalId | undefined {
  let last: import('ydoc-shared/yjsModel').ExternalId | undefined
  for (const [nodeId] of graphStore.db.nodeOutputPorts.allForward()) {
    last = nodeId
  }
  return last
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
 * Convert the engine's raw "Cannot encode class X to byte array." failures — which fire when
 * the model returns a non-Text value through `evaluateExpression` — into an actionable hint
 * that names the offending type and points at the standard remedies. Pass other messages
 * through unchanged.
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
