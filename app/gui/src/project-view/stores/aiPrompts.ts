/** @file Renderer-side queue and placeholder nodes for in-flight AI component prompts. */

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

/** `failed` is the brief post-error display before the placeholder is removed. */
export type PendingStatus = 'queued' | 'running' | 'failed'

/** A pending AI prompt rendered as a placeholder node on the graph. */
export interface AiPending {
  readonly id: string
  /** Echoed in `aiProgress` events and used as the cancel key over IPC. */
  readonly requestId: string
  /** Captured at enqueue so a navigation-away still commits the new node into the right method. */
  readonly methodId: ExternalId
  /** Captured at enqueue for the same reason as {@link methodId}. */
  readonly methodName: string
  readonly position: Vec2
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  status: PendingStatus
  /** Live status text shown above the placeholder; updated by progress events. */
  statusText: string
}

export interface EnqueueArgs {
  readonly prompt: string
  readonly sourceIdentifier: string | undefined
  readonly methodId: ExternalId
  readonly methodName: string
  readonly position: Vec2
}

export type AiPromptsStore = ReturnType<typeof aiPromptsStoreFactory>

const STATUS_TEXT_MAX_CHARS = 120
const FAILED_DISPLAY_MS = 3_000
const QUEUED_LABEL = 'Queued…'
const STARTED_LABEL = 'Thinking…'

/**
 * Owns the placeholder nodes for in-flight AI prompts and serializes their dispatch to the
 * Electron main process — only one request is in flight at a time because the `claude` CLI is
 * single-stream stdin/stdout. Live progress events update each placeholder's `statusText`;
 * cancelling either drops a still-queued entry or sends a cancel IPC for a running one. The
 * store is local-only (not broadcast over Yjs awareness).
 */
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
        // Tool args (raw expressions / file paths) tend to be cryptic to the user — log them to the
        // web console for debugging and let the placeholder keep showing the model's last text
        // narration, which describes what the agent is actually trying to do.
        console.log(
          `[AI] ${event.toolName}${event.description ? `: ${event.description}` : ''}`,
        )
        break
    }
  }

  function findByRequestId(requestId: string): AiPending | undefined {
    for (const entry of entries.values()) {
      if (entry.requestId === requestId) return entry
    }
    return undefined
  }

  /** Add a placeholder for a new AI prompt and lazily kick the dispatcher. */
  function enqueue(args: EnqueueArgs): string {
    const id = newId()
    const requestId = newId()
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
   * Drop a placeholder. For a `running` entry the cancel IPC is sent and removal happens later
   * when the dispatcher's pending dispatch resolves with a cancellation `Err` — keeps the
   * one-request-one-settle bookkeeping linear.
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
