import { useCurrentProject, useGraphStore } from '$/components/WithCurrentProject.vue'
import type { CurrentProjectStore } from '$/components/WithCurrentProject.vue'
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
    void handleToolCall(request, graphStore, currentProject).then((reply) => {
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
    const result = await projectStore.queuedExecuteExpression(anchor, request.expression)
    if (result == null) {
      return fail('expression evaluation returned no result')
    }
    if (!result.ok) {
      return fail(formatLsError(result.error))
    }
    return { requestId: request.requestId, result: { ok: true, value: result.value } }
  } catch (err) {
    return fail(formatLsError(err))
  }
}

function lastInScopeNodeId(graphStore: GraphStore): import('ydoc-shared/yjsModel').ExternalId | undefined {
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
  if (typeof err === 'object' && 'payload' in err && typeof (err as { payload: unknown }).payload === 'string') {
    return (err as { payload: string }).payload
  }
  try {
    return JSON.stringify(err)
  } catch {
    return String(err)
  }
}
