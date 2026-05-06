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
 *
 * ## `executionContext/executeExpression` semantics — pick the right anchor
 *
 * The LS treats `expressionId` (here called the "anchor") as a program point: the snippet runs
 * with the local + global symbols that are in scope **at that point**, as if a breakpoint paused
 * execution there and we typed the snippet into the REPL. The exact node you anchor on therefore
 * determines what the snippet can see. For
 *
 *     main =
 *         operator1 = 42
 *         operator2 = operator1 + 1
 *
 *     fun1 x = x.to_text
 *
 * the visible-symbols rules are:
 *
 * - Anchor on the **method body** (`main`'s body block): all bindings of the method are visible.
 *   Here that's `operator1`, `operator2`, and the module-level `fun1`.
 * - Anchor on a **binding statement** (e.g. the whole `operator2 = operator1 + 1` line): the
 *   binding itself plus every previously declared binding is visible. Same as above for this
 *   example because `operator2` is the last line.
 * - Anchor on an **arbitrary expression** (e.g. the RHS `operator1 + 1`): only symbols in scope
 *   *at that expression* are visible — `operator1` and `fun1`, but **not** `operator2`, because
 *   the assignment `operator2 := …` hasn't run yet at the program point of the RHS.
 *
 * This last rule is the trap: the GUI's "node id" for `operator2 = …` is the externalId of the
 * RHS expression, not of the binding statement. Anchoring on a node id therefore puts the snippet
 * at the RHS program point, where the node's own binding is still uninitialized — references like
 * `operator2.column_names` come back as `Uninitialized value`. We anchor on the method body
 * instead, mirroring `ComponentBrowser.vue`'s preview path; that gives the AI tool the strongest
 * scope (all method bindings visible) regardless of which node it asks about.
 *
 * Reference: the historical write-up of this rule lived in
 * `docs/language-server/protocol-language-server.md` under `executionContext/executeExpression`
 * (commit `c30a0f6`, lines 3888-3922) before that endpoint was migrated off JSON-RPC to the
 * visualization subdoc; the semantics on the engine side are unchanged.
 */
export function useAiToolHandler(
  graphStore: GraphStore = useGraphStore(),
  currentProject: CurrentProjectStore | undefined = useCurrentProject(true),
): void {
  const electronApi = typeof window === 'undefined' ? undefined : window.api
  if (electronApi == null) return
  let nextCallId = 0
  const dispose = electronApi.ai.onToolCall((request) => {
    const callId = ++nextCallId
    const t0 = performance.now()
    console.log(`Tool called [#${callId}]`, request.expression)
    void handleToolCall(request, graphStore, currentProject).then((reply) => {
      const elapsedMs = Math.round(performance.now() - t0)
      console.log(`Tool response [#${callId}, ${elapsedMs}ms]`, reply.result)
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
  // Anchor on the method body's externalId — the same anchor `ComponentBrowser.vue` preview
  // evaluation uses (see `previewDataSource` there). The LS interprets `expressionId` as the
  // program point at which the snippet runs, so anchoring on the body gives the snippet a scope
  // populated with every binding the method defines, exactly the scope into which a generated
  // node would land. See the `executionContext/executeExpression` notes at the top of this file
  // for why other obvious anchors (graph node ids, "last in-scope binding") fall short.
  if (!graphStore.currentMethod.ast.ok) {
    return fail('current method has no parsed AST')
  }
  const body = graphStore.currentMethod.ast.value.body
  if (body == null) {
    return fail('current method has no body to anchor scope')
  }
  const anchor = body.externalId
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
