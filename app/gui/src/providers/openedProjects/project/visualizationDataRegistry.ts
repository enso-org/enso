import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { reactive } from 'vue'
import type {
  ExpressionUpdatePayload,
  MethodCall,
  ProfilingInfo,
  Uuid,
} from 'ydoc-shared/languageServerTypes'
import {
  isInFrameRequest,
  Visualizations,
  VisualizationSlotView,
  type VisRequestId,
  type VisualizationId,
} from 'ydoc-shared/visualizations'
import type { DistributedProject } from 'ydoc-shared/yjsModel'
import type * as Y from 'yjs'

export interface ExpressionInfo {
  typename: string | undefined
  methodCall: MethodCall | undefined
  payload: ExpressionUpdatePayload
  profilingInfo: ProfilingInfo[]
}

/**
 * Holds the latest available visualization response per visualization id.
 *
 * Reads slots from the vis subdoc. For each `visualizationId`,
 * the registry picks the slot with the largest `createdAt`
 * that has reached a terminal state (`ready` or `failed`), decodes its
 * response bytes as UTF-8 JSON (to keep the existing `getRawData` contract),
 * and exposes it reactively.
 *
 * Stale-but-compatible rendering falls out of this design automatically:
 * while a newer slot is still `pending`, the most recent previously-ready
 * slot remains the registry's answer for that visualization id.
 */
export class VisualizationDataRegistry {
  private readonly visualizationValues: Map<Uuid, Result<string> | null>
  private vis: Visualizations | null = null
  private unobserveVisDoc: (() => void) | null = null
  private unobserveSlots: (() => void) | null = null

  /** TODO: Add docs */
  constructor(projectModel: DistributedProject) {
    this.visualizationValues = reactive(new Map())

    this.unobserveVisDoc = projectModel.observeVisualizationsDoc((doc) => {
      this.rebindToDoc(doc)
    })
  }

  private rebindToDoc(doc: Y.Doc | null) {
    this.unobserveSlots?.()
    this.unobserveSlots = null
    if (!doc) {
      this.vis = null
      this.visualizationValues.clear()
      return
    }
    // Subdoc contents only sync once it is loaded; see the corresponding
    // comment in `executionContext.ts::subscribeToVisualizationsDoc`. Safe
    // to call more than once because Yjs treats load() as idempotent.
    doc.load()
    const vis = new Visualizations(doc)
    this.vis = vis
    const observer = () => this.recompute()
    vis.slots.observeDeep(observer)
    this.unobserveSlots = () => vis.slots.unobserveDeep(observer)
    this.recompute()
  }

  /**
   * Walks all slots and picks the most-recent-terminal slot per vis id.
   *
   * Skips `inFrame` slots: those are one-shot `executeExpression` requests
   * whose response the caller awaits directly via the slot, with no need to
   * cache by `visualizationId`.
   */
  private recompute() {
    const vis = this.vis
    if (!vis) return

    const latest = new Map<string, { view: VisualizationSlotView; createdAt: number }>()
    for (const view of vis.entries()) {
      if (view.status !== 'ready' && view.status !== 'failed') continue
      if (isInFrameRequest(view.request)) continue
      const visIdStr = (view.visualizationId as string | undefined) ?? undefined
      if (!visIdStr) continue
      const createdAt = view.createdAt ?? 0
      const existing = latest.get(visIdStr)
      if (!existing || existing.createdAt < createdAt) {
        latest.set(visIdStr, { view, createdAt })
      }
    }

    // Insert / update entries for each visualization id we saw.
    for (const [visId, { view }] of latest) {
      this.commit(visId as Uuid, view)
    }

    // Drop entries for vis ids no longer present in any slot.
    for (const key of Array.from(this.visualizationValues.keys())) {
      if (!latest.has(key)) this.visualizationValues.delete(key)
    }
  }

  private commit(visId: Uuid, view: VisualizationSlotView) {
    const current = this.visualizationValues.get(visId)
    if (view.status === 'ready') {
      const bytes = view.response
      if (bytes == null) return
      let text: string
      try {
        text = new TextDecoder('utf-8', { fatal: false }).decode(bytes)
      } catch {
        this.visualizationValues.set(visId, Err('Failed to decode visualization response as UTF-8'))
        return
      }
      if (!current?.ok || current.value !== text) {
        this.visualizationValues.set(visId, Ok(text))
      }
    } else if (view.status === 'failed') {
      const message = view.failure?.message ?? 'Visualization evaluation failed'
      if (current == null || current.ok || current.error.payload !== message) {
        this.visualizationValues.set(visId, Err(message))
      }
    }
  }

  /**
   * Latest decoded visualization response for the given id, or null if none
   * has been received (the returned value may be a previously-ready result
   * from a superseded-but-compatible slot).
   */
  getRawData(visualizationId: Uuid): Result<string> | null {
    return this.visualizationValues.get(visualizationId) ?? null
  }

  /** Release observers. */
  dispose() {
    this.unobserveSlots?.()
    this.unobserveSlots = null
    this.unobserveVisDoc?.()
    this.unobserveVisDoc = null
  }
}

// Re-exports to stabilize the registry's import surface for unit tests.
export type { VisRequestId, VisualizationId }
