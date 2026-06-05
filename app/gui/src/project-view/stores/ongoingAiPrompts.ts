/** @file Renderer-side queue and placeholder nodes for in-flight AI component prompts. */

import {
  type CurrentProjectStore,
  useCurrentProject,
  useGraphStore,
  useProjectNames,
} from '$/components/WithCurrentProject.vue'
import type { GraphStore, NodeId } from '$/providers/openedProjects/graph'
import { proxyRefs } from '$/utils/reactivity'
import { DEFAULT_NODE_SIZE } from '@/components/ComponentBrowser/placement'
import {
  createAiNode,
  isAiAssignment,
  readAiCallTarget,
  readAiPrompt,
  updateAiNode,
} from '@/components/GraphEditor/aiNode'
import { useAI } from '@/composables/ai'
import { createContextStore } from '@/providers'
import { Ast } from '@/util/ast'
import { nodeDocumentationText } from '@/util/ast/node'
import { Rect } from '@/util/data/rect'
import type { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import type { AiComponentResponse, AiEditContext, AiProgressEvent } from 'enso-common/src/ai'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, onScopeDispose, reactive } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

/** A `failed` placeholder stays on screen until the user dismisses it via the cancel button. */
export type PendingStatus = 'queued' | 'running' | 'failed'

/** A pending AI prompt rendered as a placeholder node on the graph. */
export interface AiPending {
  readonly id: string
  /** Echoed in `aiProgress` events and used as the cancel key over IPC. */
  readonly requestId: string
  /** Captured at enqueue so a navigation-away still commits the new node into the right method. */
  readonly methodId: ExternalId
  /**
   * Captured at enqueue: the externalId of the current method's *body*, used as the LS scope
   * anchor for mid-turn `evaluateExpression` tool calls. Pinned per entry so a tool call evaluates
   * in the scope the agent was prompted with, even if the user navigates between methods while
   * the turn is in flight.
   */
  readonly methodBodyId: ExternalId
  /** Captured at enqueue for the same reason as {@link methodId}. */
  readonly methodName: string
  readonly position: Vec2
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  status: PendingStatus
  /**
   * `true` once {@link runEntry} has called the IPC dispatch for this entry. The user-visible
   * `status` may still be `'queued'` until the main process emits `'started'`, but cancellation
   * needs to send the IPC any time we've entered the dispatch — otherwise main keeps processing.
   */
  dispatched: boolean
  /** Live status text shown above the placeholder; updated by progress events. */
  statusText: string
  /**
   * When set, this entry rewrites an existing AI-generated node rather than creating a new one.
   * Captured at enqueue time so the prompt + previous definition sent to the agent are pinned
   * regardless of subsequent edits to the underlying AST.
   */
  editTarget?: {
    readonly nodeId: NodeId
    readonly previousPrompt: string
    readonly previousDefinition: string | undefined
  }
}

export interface EnqueueArgs {
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  readonly methodId: ExternalId
  readonly methodBodyId: ExternalId
  readonly methodName: string
  readonly position: Vec2
}

export interface EnqueueEditArgs {
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  readonly methodId: ExternalId
  readonly methodBodyId: ExternalId
  readonly methodName: string
  readonly editNodeId: NodeId
}

export type OngoingAiPromptsStore = ReturnType<typeof ongoingAiPromptsStoreFactory>

/**
 * Dependencies the {@link ongoingAiPromptsStoreFactory} reaches for. Surfaced as a parameter
 * so unit tests can wire in fakes; the production `createContextStore` lambda below resolves the
 * real stores. Each entry is `Pick<>`-ed to just the methods actually called so test fakes need
 * not re-implement the full upstream surface.
 */
export interface OngoingAiPromptsDeps {
  readonly graphStore: Pick<
    GraphStore,
    'db' | 'currentMethod' | 'registerExtraOccupiedAreas' | 'generateLocallyUniqueIdent'
  >
  readonly module: CurrentProjectStore['module']
  readonly ai: Pick<ReturnType<typeof useAI>, 'dispatch'>
  readonly toastError: Pick<ReturnType<typeof useToast.error>, 'show'>
  readonly electronApi: typeof window.api | undefined
}

const STATUS_TEXT_MAX_CHARS = 120
const QUEUED_LABEL = 'Waiting…'
const STARTED_LABEL = 'Thinking…'

function queuedPositionLabel(position: number): string {
  return `Waiting (#${position})`
}

/**
 * Owns the placeholder nodes for in-flight AI prompts and serializes their dispatch to the
 * Electron main process — only one request is in flight at a time because the `claude` CLI is
 * single-stream stdin/stdout. Live progress events update each placeholder's `statusText`;
 * cancelling either drops a still-queued/failed entry or sends a cancel IPC for a running one.
 * The store is local-only (not broadcast over Yjs awareness).
 */
export function ongoingAiPromptsStoreFactory(deps: OngoingAiPromptsDeps) {
  const { graphStore, module, ai, toastError, electronApi } = deps

  const entries = reactive(new Map<string, AiPending>())
  let dispatching = false

  if (electronApi != null) {
    const dispose = electronApi.ai.onProgress(handleProgress)
    onScopeDispose(dispose)
  }

  // Tear down on scope dispose: cancel any IPC-dispatched entry so a stale dispatch can't
  // resolve and commit a node into a different module after this store's owner unmounts.
  // Renderer-only queued entries (no IPC sent yet) are dropped silently by `entries.clear()`.
  onScopeDispose(() => {
    if (electronApi != null) {
      for (const entry of entries.values()) {
        if (entry.dispatched && entry.status !== 'failed') {
          electronApi.ai.cancel(entry.requestId)
        }
      }
    }
    entries.clear()
  })

  // Expose placeholder positions to the graph store so node placement avoids overlapping with
  // in-flight AI prompts. Read snapshot inside the source so the graph store's computed picks
  // up changes to `entries` reactively.
  const unregisterPlaceholderRects = graphStore.registerExtraOccupiedAreas(() =>
    Array.from(entries.values(), (e) => new Rect(e.position, DEFAULT_NODE_SIZE)),
  )
  onScopeDispose(unregisterPlaceholderRects)

  function handleProgress(event: AiProgressEvent): void {
    const target = findByRequestId(event.requestId)
    if (target == null) return
    switch (event.kind) {
      case 'queued':
        // The IPC reached the main process. Collapse any "(#N)" position label set at enqueue
        // — that count is renderer-side queue position and is no longer accurate once we're
        // through the local queue.
        if (target.status === 'queued') target.statusText = QUEUED_LABEL
        break
      case 'started':
        target.status = 'running'
        target.statusText = STARTED_LABEL
        break
      case 'text': {
        const trimmed = event.text.trim()
        if (trimmed.length > 0) target.statusText = truncate(trimmed, STATUS_TEXT_MAX_CHARS)
        break
      }
      case 'tool':
        // Tool args (raw expressions / file paths) tend to be cryptic to the user — surface them
        // in the web console for debugging and let the placeholder keep showing the model's last
        // text narration, which describes what the agent is actually trying to do.
        console.log(`[AI] ${event.toolName}`, event.input)
        break
    }
  }

  function findByRequestId(requestId: string): AiPending | undefined {
    for (const entry of entries.values()) {
      if (entry.requestId === requestId) return entry
    }
    return undefined
  }

  /** Add a placeholder for a new AI prompt and kick the dispatcher. */
  function enqueue(args: EnqueueArgs): string {
    const id = newId()
    const requestId = newId()
    const ahead = countActive()
    const placeholder: AiPending = {
      id,
      requestId,
      methodId: args.methodId,
      methodBodyId: args.methodBodyId,
      methodName: args.methodName,
      position: args.position,
      prompt: args.prompt,
      sourceIdentifier: args.sourceIdentifier,
      status: 'queued',
      dispatched: false,
      statusText: ahead === 0 ? QUEUED_LABEL : queuedPositionLabel(ahead + 1),
    }
    entries.set(id, placeholder)
    void kickDispatcher()
    return id
  }

  /**
   * Enqueue an edit of an existing AI node. Captures the previous prompt and the previous
   * function definition source at enqueue time so the agent sees a stable snapshot even if the
   * AST is mutated while the request is in flight. Returns the placeholder id.
   */
  function enqueueEdit(args: EnqueueEditArgs): Result<string> {
    const node = graphStore.db.nodeIdToNode.get(args.editNodeId)
    if (!node || !isAiAssignment(node.outerAst)) {
      return Err('Node is no longer an AI-generated component.')
    }
    const previousPrompt = readAiPrompt(nodeDocumentationText(node)) ?? ''
    const topLevel = module.value.root
    let previousDefinition: string | undefined = undefined
    if (topLevel != null && node.outerAst instanceof Ast.Assignment) {
      previousDefinition = readAiCallTarget(node.outerAst, topLevel)?.definitionCode
    }
    const id = newId()
    const requestId = newId()
    const ahead = countActive()
    const placeholder: AiPending = {
      id,
      requestId,
      methodId: args.methodId,
      methodBodyId: args.methodBodyId,
      methodName: args.methodName,
      position: node.position,
      prompt: args.prompt,
      sourceIdentifier: args.sourceIdentifier,
      status: 'queued',
      dispatched: false,
      statusText: ahead === 0 ? QUEUED_LABEL : queuedPositionLabel(ahead + 1),
      editTarget: {
        nodeId: args.editNodeId,
        previousPrompt,
        previousDefinition,
      },
    }
    entries.set(id, placeholder)
    void kickDispatcher()
    return Ok(id)
  }

  /**
   * Set of node ids that should be visually hidden while an edit-AI-prompt request is in flight
   * — the placeholder stands in for them. Excludes `failed` entries so a parked failure leaves
   * the underlying node visible (the user can read its current state while resolving the error).
   */
  const hiddenNodeIds = computed<ReadonlySet<NodeId>>(() => {
    const ids = new Set<NodeId>()
    for (const entry of entries.values()) {
      if (entry.editTarget != null && entry.status !== 'failed') {
        ids.add(entry.editTarget.nodeId)
      }
    }
    return ids
  })

  /**
   * Drop a placeholder. Three explicit branches:
   * - `failed`: the dispatch already settled with an error and the placeholder is parked for
   *   user-visible diagnosis. Clicking cancel just dismisses it.
   * - `dispatched` (and not failed): an IPC is already in flight — send a cancel and let the
   *   reply (a cancellation `Err`) drop the entry through {@link runEntry}, keeping
   *   one-request-one-settle bookkeeping linear.
   * - otherwise (renderer-side queued, no IPC sent yet): drop locally.
   */
  function cancel(id: string): void {
    const entry = entries.get(id)
    if (entry == null) return
    if (entry.status === 'failed') {
      entries.delete(id)
      return
    }
    if (entry.dispatched) {
      electronApi?.ai.cancel(entry.requestId)
      return
    }
    entries.delete(id)
  }

  /**
   * Re-queue the same prompt: cancel any in-flight dispatch for this entry and enqueue a fresh
   * placeholder with the same args at the same position. The caller hides the refresh button on
   * `queued` entries, so this is only invoked from `running` or `failed`.
   *
   * For edit entries, this recaptures `previousPrompt`/`previousDefinition` from the live AST — a
   * deliberate "fresh start" so any manual edits the user made while the placeholder was failed
   * feed into the retry.
   *
   * Unlike {@link cancel}, which lets {@link runEntry}'s cancellation reply delete the entry,
   * this deletes the old entry synchronously. The `entries.has(entry.id)` guard inside
   * {@link runEntry} keeps the late-arriving cancellation reply silent.
   */
  function refresh(id: string): void {
    const entry = entries.get(id)
    if (entry == null) return
    if (entry.editTarget != null) {
      const result = enqueueEdit({
        prompt: entry.prompt,
        sourceIdentifier: entry.sourceIdentifier,
        methodId: entry.methodId,
        methodBodyId: entry.methodBodyId,
        methodName: entry.methodName,
        editNodeId: entry.editTarget.nodeId,
      })
      if (!result.ok) {
        toastError.show(result.error.message('Cannot retry AI prompt'))
        return
      }
    } else {
      enqueue({
        prompt: entry.prompt,
        sourceIdentifier: entry.sourceIdentifier,
        methodId: entry.methodId,
        methodBodyId: entry.methodBodyId,
        methodName: entry.methodName,
        position: entry.position,
      })
    }
    if (entry.dispatched && entry.status !== 'failed') {
      electronApi?.ai.cancel(entry.requestId)
    }
    entries.delete(id)
  }

  async function kickDispatcher(): Promise<void> {
    if (dispatching) return
    dispatching = true
    try {
      let next = pickNext()
      while (next != null) {
        await runEntry(next)
        next = pickNext()
      }
    } finally {
      dispatching = false
    }
  }

  function pickNext(): AiPending | undefined {
    for (const entry of entries.values()) {
      if (entry.status === 'queued' && !entry.dispatched) return entry
    }
    return undefined
  }

  async function runEntry(entry: AiPending): Promise<void> {
    // The user-visible status stays `'queued'` until the main process emits `'started'`. We mark
    // `dispatched` here so cancel() routes through IPC instead of dropping locally — the IPC has
    // already left the renderer.
    entry.dispatched = true
    const editContextPayload: AiEditContext | undefined =
      entry.editTarget != null ?
        {
          previousPrompt: entry.editTarget.previousPrompt,
          ...(entry.editTarget.previousDefinition != null ?
            { previousDefinition: entry.editTarget.previousDefinition }
          : {}),
        }
      : undefined
    let result
    try {
      result = await ai.dispatch(
        entry.prompt,
        entry.sourceIdentifier,
        entry.requestId,
        editContextPayload,
      )
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err)
      handleFailure(entry, `AI component generation failed: ${message}`)
      return
    }
    if (!entries.has(entry.id)) {
      // Placeholder was removed during dispatch (cancel race). Drop the result silently.
      return
    }
    if (!result.ok) {
      const payload = result.error.payload
      if (typeof payload === 'string' && payload.toLowerCase().includes('cancelled by user')) {
        // Silent cleanup for user-initiated cancels — no toast.
        entries.delete(entry.id)
        return
      }
      handleFailure(entry, result.error.message('AI component generation failed'))
      return
    }
    commit(entry, result.value)
  }

  function handleFailure(entry: AiPending, message: string): void {
    if (!entries.has(entry.id)) return
    toastError.show(message)
    entry.status = 'failed'
    entry.statusText = truncate(message, STATUS_TEXT_MAX_CHARS)
  }

  function commit(entry: AiPending, response: AiComponentResponse): void {
    const topLevel = module.value.root
    if (topLevel == null) {
      handleFailure(entry, 'Cannot create AI component: module root not loaded.')
      return
    }
    if (entry.editTarget != null) {
      const node = graphStore.db.nodeIdToNode.get(entry.editTarget.nodeId)
      if (!node || !(node.outerAst instanceof Ast.Assignment)) {
        handleFailure(entry, 'AI node was removed before the update could land.')
        return
      }
      const assignmentId = node.outerAst.id
      const editResult = module.value.edit((edit) => {
        const mutableTopLevel = edit.getVersion(topLevel)
        const mutableAssignment = edit.get(assignmentId)
        if (!(mutableAssignment instanceof Ast.MutableAssignment)) {
          return Err('Cannot resolve edited AI node in module edit.')
        }
        return updateAiNode({
          edit,
          topLevel: mutableTopLevel,
          assignment: mutableAssignment,
          currentMethodName: entry.methodName,
          prompt: entry.prompt,
          response,
        })
      })
      if (!editResult.ok) {
        handleFailure(entry, editResult.error.message('Cannot update AI component'))
        return
      }
      entries.delete(entry.id)
      return
    }
    const editResult = module.value.edit((edit) =>
      createAiNode({
        edit,
        topLevel: edit.getVersion(topLevel),
        currentMethodName: entry.methodName,
        binding: graphStore.generateLocallyUniqueIdent('ai_component'),
        position: entry.position,
        payload: { prompt: entry.prompt, response },
      }),
    )
    if (!editResult.ok) {
      handleFailure(entry, editResult.error.message('Cannot create AI component'))
      return
    }
    entries.delete(entry.id)
  }

  /** Placeholders that belong to the method currently visible in the graph editor. */
  const entriesForCurrentMethod = computed<readonly AiPending[]>(() => {
    if (!graphStore.currentMethod.ast.ok) return []
    const methodId = graphStore.currentMethod.ast.value.externalId
    const list: AiPending[] = []
    for (const entry of entries.values()) {
      if (entry.methodId === methodId) list.push(entry)
    }
    return list
  })

  /** Active = not-yet-settled. Used to render the "Queued (N pending)" count on new entries. */
  function countActive(): number {
    let n = 0
    for (const entry of entries.values()) {
      if (entry.status !== 'failed') n++
    }
    return n
  }

  return proxyRefs({
    enqueue,
    enqueueEdit,
    cancel,
    refresh,
    findByRequestId,
    entriesForCurrentMethod,
    hiddenNodeIds,
  })
}

export const [provideOngoingAiPrompts, useOngoingAiPrompts] = createContextStore(
  'ongoingAiPrompts',
  () => {
    const graphStore = useGraphStore()
    const projectNames = useProjectNames()
    const { module } = useCurrentProject()
    return ongoingAiPromptsStoreFactory({
      graphStore,
      module,
      ai: useAI(graphStore, projectNames),
      toastError: useToast.error(),
      electronApi: typeof window === 'undefined' ? undefined : window.api,
    })
  },
)

function truncate(value: string, max: number): string {
  if (value.length <= max) return value
  return `${value.slice(0, max - 1)}…`
}

function newId(): string {
  return crypto.randomUUID()
}
