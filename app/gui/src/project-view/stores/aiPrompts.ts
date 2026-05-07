/**
 * @file Renderer-side queue for in-flight AI component prompts.
 *
 * The Component Browser closes immediately on submit; each AI request becomes a "pending node"
 * placeholder rendered on the graph. This store owns those placeholders and serializes the
 * dispatch to the Electron main process — at most one request is in flight at a time, since the
 * `claude` CLI subprocess is single-stream stdin/stdout (see `app/electron-client/CLAUDE.md`).
 *
 * Live progress events from the main process update each placeholder's `statusText`; cancelling
 * a placeholder either drops it from the queue (if not yet running) or sends a cancel IPC (if
 * running). On success the placeholder is removed and the AST is committed via `createAiNode`.
 *
 * The store is local-only — pending placeholders are NOT broadcast over Yjs awareness. A user's
 * in-flight prompts are their own affair; cross-window visibility would invite distracting
 * noise without meaningful collaboration value.
 */

import { useCurrentProject, useGraphStore, useProjectNames } from '$/components/WithCurrentProject.vue'
import { proxyRefs } from '$/utils/reactivity'
import { useAI } from '@/components/ComponentBrowser/ai'
import { createAiNode } from '@/components/GraphEditor/aiNode'
import { createContextStore } from '@/providers'
import type { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import type { AiComponentResponse, AiProgressEvent } from 'enso-common/src/ai'
import { computed, onScopeDispose, reactive } from 'vue'
import type { ExternalId } from 'ydoc-shared/yjsModel'

/**
 * Lifecycle states for a pending placeholder. `queued` means the dispatcher has not yet sent
 * the request to the main process; `running` means the IPC is in flight; `failed` is a brief
 * post-error display state before the placeholder is removed.
 */
export type PendingStatus = 'queued' | 'running' | 'failed'

/** A single pending AI prompt rendered as a placeholder node on the graph. */
export interface AiPending {
  /** Local identifier for the placeholder; also the key into the store's entries map. */
  readonly id: string
  /** Identifier echoed in `aiProgress` events and used as the cancel key over IPC. */
  readonly requestId: string
  /**
   * `externalId` of the method the placeholder belongs to — used to filter which method's graph
   * displays which placeholder. Captured at enqueue so navigating away doesn't move the
   * placeholder; navigating back brings it into view.
   */
  readonly methodId: ExternalId
  /**
   * Method name captured at enqueue, used at commit time to find the destination function in
   * the module's top-level. Persisting the name (rather than re-reading the current method)
   * means a navigation-away after enqueue still commits to the right method.
   */
  readonly methodName: string
  /** Position the AI node will occupy once committed. */
  readonly position: Vec2
  /** Original natural-language prompt; surfaced as the bubble's title/tooltip. */
  readonly prompt: string
  /** Source binding the user dropped onto the prompt, if any. */
  readonly sourceIdentifier: string | undefined
  status: PendingStatus
  /** Live status text shown above the placeholder; updated by progress events. */
  statusText: string
}

/** Arguments to {@link AiPromptsStore.enqueue}. */
export interface EnqueueArgs {
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  readonly methodId: ExternalId
  readonly methodName: string
  readonly position: Vec2
}

/** Public API of the AI prompts store; exported for typing of consumers. */
export type AiPromptsStore = ReturnType<typeof aiPromptsStoreFactory>

const STATUS_TEXT_MAX_CHARS = 120
const FAILED_DISPLAY_MS = 3_000
const QUEUED_LABEL = 'Queued…'
const STARTED_LABEL = 'Thinking…'

function aiPromptsStoreFactory() {
  const graphStore = useGraphStore()
  const projectNames = useProjectNames()
  const { module } = useCurrentProject()
  const ai = useAI(graphStore, projectNames)
  const toastError = useToast.error()

  const entries = reactive(new Map<string, AiPending>())
  let dispatching = false

  const electronApi = typeof window === 'undefined' ? undefined : window.api
  if (electronApi != null) {
    const dispose = electronApi.ai.onProgress(handleProgress)
    onScopeDispose(dispose)
  }

  function handleProgress(event: AiProgressEvent): void {
    const target = findByRequestId(event.requestId)
    if (target == null) return
    switch (event.kind) {
      case 'queued':
        target.statusText = QUEUED_LABEL
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
        target.statusText =
          event.description ?
            `${event.toolName}: ${truncate(event.description, STATUS_TEXT_MAX_CHARS)}`
          : event.toolName
        break
    }
  }

  function findByRequestId(requestId: string): AiPending | undefined {
    for (const entry of entries.values()) {
      if (entry.requestId === requestId) return entry
    }
    return undefined
  }

  /**
   * Add a placeholder for a new AI prompt and (lazily) kick the dispatcher. Returns the
   * local placeholder id so the caller can pass it to {@link cancel} if needed.
   */
  function enqueue(args: EnqueueArgs): string {
    const id = newId()
    const requestId = newId()
    // Initial label reflects the queue state at enqueue time. Once the request reaches the head
    // of the queue, the dispatcher overwrites it via the `started` event and subsequent
    // progress events.
    const ahead = countActive()
    const placeholder: AiPending = {
      id,
      requestId,
      methodId: args.methodId,
      methodName: args.methodName,
      position: args.position,
      prompt: args.prompt,
      sourceIdentifier: args.sourceIdentifier,
      status: 'queued',
      statusText: ahead === 0 ? QUEUED_LABEL : `Queued (${ahead + 1} pending)`,
    }
    entries.set(id, placeholder)
    void kickDispatcher()
    return id
  }

  /**
   * Drop a placeholder. Behavior depends on its lifecycle state:
   * - `queued`: removed immediately. The dispatcher's `pickNext` skips deleted entries, so no
   *   IPC ever fires for this prompt.
   * - `running`: a cancel IPC is sent to the main process. The dispatcher's awaited `dispatch`
   *   eventually resolves with a cancellation `Err`; `runEntry` then removes the placeholder
   *   silently. We do NOT remove synchronously here so the dispatcher's bookkeeping stays
   *   linear (one request → one settle → one removal).
   * - `failed`: removed immediately, dismissing the post-error display early.
   */
  function cancel(id: string): void {
    const entry = entries.get(id)
    if (entry == null) return
    if (entry.status === 'running') {
      electronApi?.ai.cancel(entry.requestId)
      return
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
      if (entry.status === 'queued') return entry
    }
    return undefined
  }

  async function runEntry(entry: AiPending): Promise<void> {
    entry.status = 'running'
    entry.statusText = STARTED_LABEL
    let result
    try {
      result = await ai.dispatch(entry.prompt, entry.sourceIdentifier, entry.requestId)
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
    setTimeout(() => entries.delete(entry.id), FAILED_DISPLAY_MS)
  }

  function commit(entry: AiPending, response: AiComponentResponse): void {
    const topLevel = module.value.root
    if (topLevel == null) {
      handleFailure(entry, 'Cannot create AI component: module root not loaded.')
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

  /** Count placeholders that haven't completed yet — used to show "Queued (N pending)". */
  function countActive(): number {
    let n = 0
    for (const entry of entries.values()) {
      if (entry.status !== 'failed') n++
    }
    return n
  }

  return proxyRefs({
    enqueue,
    cancel,
    entriesForCurrentMethod,
  })
}

export const [provideAiPrompts, useAiPrompts] = createContextStore('aiPrompts', aiPromptsStoreFactory)

function truncate(value: string, max: number): string {
  if (value.length <= max) return value
  return `${value.slice(0, max - 1)}…`
}

function newId(): string {
  return crypto.randomUUID()
}
