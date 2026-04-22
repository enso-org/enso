/**
 * Shared types and helpers for the visualization subdoc exchanged between the
 * client, ydoc-server, and the Language Server.
 *
 * The vis subdoc holds a single top-level `slots: Y.Map<string, Y.Map<unknown>>`.
 * Outer map: keyed by request id (fresh UUID per attach/modify, immutable).
 * Inner map: one slot, with literal field-name string keys.
 */

import * as Y from 'yjs'
import type { ContextId, Diagnostic, ExpressionId, LSMethodPointer } from './languageServerTypes'
import type { Uuid } from './yjsModel'

declare const brandVisRequestId: unique symbol
export type VisRequestId = Uuid & { [brandVisRequestId]: never }

declare const brandVisualizationId: unique symbol
export type VisualizationId = Uuid & { [brandVisualizationId]: never }

/**
 * The expression body carried inside a `VisRequestPreprocessor`. The shape is
 * the same tagged union the Language Server accepts, with the rule that
 * exactly one tag is present per value:
 *
 * - plain `string` - evaluated as `Api.VisualizationExpression.Text` in the
 *   preprocessor module scope,
 * - `LSMethodPointer` object - evaluated as
 *   `Api.VisualizationExpression.ModuleMethod`,
 * - `{ inFrame: string }` - evaluated as `Api.VisualizationExpression.InFrame`
 *   in the node's breakpoint frame. The underlying runtime auto-detaches the
 *   visualization after exactly one response, so slots with this expression
 *   shape are **terminal on response** (`isTerminal()` returns `true` once
 *   `status === 'ready'`).
 */
export type VisExpression = string | LSMethodPointer | { inFrame: string }

/**
 * Immutable preprocessor portion of a visualization request. Matches the
 * "what" fields of the LS `VisualizationConfiguration` (module + expression +
 * positional args) but excludes the context id, which is carried on the slot
 * itself so the bridge can route without parsing the request.
 */
export interface VisRequestPreprocessor {
  visualizationModule: string
  expression: VisExpression
  positionalArgumentsExpressions?: string[]
}

/** Narrow a request to an `inFrame` expression. */
export function isInFrameRequest(
  req: VisRequestPreprocessor | undefined,
): req is VisRequestPreprocessor & { expression: { inFrame: string } } {
  return !!req && typeof req.expression === 'object' && 'inFrame' in req.expression
}

export type VisSlotStatus = 'pending' | 'ready' | 'failed'

export interface VisSlotFailure {
  message: string
  diagnostic?: Diagnostic
}

/**
 * Literal string keys of the inner slot Y.Map. Exported so the bridge, the
 * registry, and tests can reference them symbolically instead of open-coding
 * the strings at every call site.
 */
export const VIS_SLOT_FIELDS = {
  visualizationId: 'visualizationId',
  contextId: 'contextId',
  nodeExternalId: 'nodeExternalId',
  request: 'request',
  status: 'status',
  response: 'response',
  failure: 'failure',
  createdAt: 'createdAt',
} as const

export type VisSlotField = (typeof VIS_SLOT_FIELDS)[keyof typeof VIS_SLOT_FIELDS]

/** Mint a fresh request id. Every attach or modify operation gets a new one. */
export function newVisRequestId(): VisRequestId {
  return crypto.randomUUID() as VisRequestId
}

/** Read-only view over one slot in the visualizations Y.Map. */
export class VisualizationSlotView {
  constructor(
    readonly requestId: VisRequestId,
    readonly inner: Y.Map<unknown>,
  ) {}

  get visualizationId(): VisualizationId | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.visualizationId) as VisualizationId | undefined
  }
  get contextId(): ContextId | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.contextId) as ContextId | undefined
  }
  get nodeExternalId(): ExpressionId | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.nodeExternalId) as ExpressionId | undefined
  }
  get request(): VisRequestPreprocessor | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.request) as VisRequestPreprocessor | undefined
  }
  get status(): VisSlotStatus | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.status) as VisSlotStatus | undefined
  }
  get response(): Uint8Array | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.response) as Uint8Array | undefined
  }
  get failure(): VisSlotFailure | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.failure) as VisSlotFailure | undefined
  }
  get createdAt(): number | undefined {
    return this.inner.get(VIS_SLOT_FIELDS.createdAt) as number | undefined
  }

  /**
   * True once the slot will not change status again.
   *
   * `failed` is terminal for every slot. `ready` is terminal only for slots
   * whose request is an `inFrame` one-shot. The runtime auto-detaches those
   * after the first response, so the client consumes the payload and removes
   * the slot rather than expecting further updates. For persistent attaches
   * a detach is a slot deletion, not a terminal status.
   */
  isTerminal(): boolean {
    const s = this.status
    if (s === 'failed') return true
    return s === 'ready' && isInFrameRequest(this.request)
  }
}

export interface VisualizationSlotSpec {
  visualizationId: VisualizationId
  contextId: ContextId
  nodeExternalId: ExpressionId
  request: VisRequestPreprocessor
}

/**
 * Wrapper around the vis subdoc's top-level `slots` Y.Map.
 *
 * Enforces the invariants listed in the rationale doc:
 * - `request` is set once at slot creation, never updated.
 * - `response` is only written via `recordResponse` (bridge-only).
 * - Detach is a slot deletion (`removeSlot`), not a status flip. The bridge
 *   observes the deletion and emits a detach message to the LS; for one-shot
 *   `inFrame` slots the deletion is expected after the response is consumed
 *   and produces no detach message.
 */
export class Visualizations {
  readonly doc: Y.Doc
  readonly slots: Y.Map<Y.Map<unknown>>

  constructor(doc: Y.Doc) {
    this.doc = doc
    this.slots = doc.getMap<Y.Map<unknown>>('slots')
  }

  getSlot(requestId: VisRequestId): VisualizationSlotView | null {
    const inner = this.slots.get(requestId)
    return inner ? new VisualizationSlotView(requestId, inner) : null
  }

  *entries(): IterableIterator<VisualizationSlotView> {
    for (const [key, inner] of this.slots.entries()) {
      yield new VisualizationSlotView(key as VisRequestId, inner)
    }
  }

  /**
   * Create a new slot under a fresh request id. Client-side entry point for
   * every visualization kind, i.e. persistent attaches, modifies, and one-shot
   * `inFrame` evaluations all land here. `modify` is just "create a new slot,
   * detach the old one". One-shot evaluations are slots whose request carries
   * an `{ inFrame }` expression and which the client removes on `ready`.
   */
  createSlot(
    spec: VisualizationSlotSpec,
    requestId: VisRequestId = newVisRequestId(),
  ): VisRequestId {
    this.doc.transact(() => {
      const inner = new Y.Map<unknown>()
      inner.set(VIS_SLOT_FIELDS.visualizationId, spec.visualizationId)
      inner.set(VIS_SLOT_FIELDS.contextId, spec.contextId)
      inner.set(VIS_SLOT_FIELDS.nodeExternalId, spec.nodeExternalId)
      inner.set(VIS_SLOT_FIELDS.request, spec.request)
      inner.set(VIS_SLOT_FIELDS.status, 'pending' satisfies VisSlotStatus)
      inner.set(VIS_SLOT_FIELDS.createdAt, Date.now())
      this.slots.set(requestId, inner)
    })
    return requestId
  }

  /**
   * Remove a slot entirely. This is how clients detach: the bridge observes
   * the deletion and emits a detach control message to the LS (except for
   * `inFrame` one-shots, whose slot removal after response is expected and
   * produces no detach message). Idempotent.
   */
  removeSlot(requestId: VisRequestId): void {
    this.slots.delete(requestId)
  }

  /**
   * Bridge-only: record a successful response and flip status to `ready`.
   * No-ops if the slot is already terminal (either `failed` for any slot, or
   * `ready` for an `inFrame` one-shot whose response was already consumed).
   * Writing over a terminal slot would misrepresent its outcome to any
   * reactive subscriber that has already committed to the current status.
   */
  recordResponse(requestId: VisRequestId, bytes: Uint8Array): void {
    const inner = this.slots.get(requestId)
    if (!inner) return
    const view = new VisualizationSlotView(requestId, inner)
    if (view.isTerminal()) {
      console.warn(
        `Visualizations.recordResponse: dropping response for terminal slot ${requestId} ` +
          `(status=${view.status})`,
      )
      return
    }
    this.doc.transact(() => {
      inner.set(VIS_SLOT_FIELDS.response, bytes)
      inner.set(VIS_SLOT_FIELDS.status, 'ready' satisfies VisSlotStatus)
    })
  }

  /**
   * Bridge-only: record a failure and flip status to `failed`. No-ops if the
   * slot is already terminal so a late failure message cannot overwrite a
   * slot the client has already observed as `ready` (one-shot) or as a
   * different failure.
   */
  recordFailure(requestId: VisRequestId, failure: VisSlotFailure): void {
    const inner = this.slots.get(requestId)
    if (!inner) return
    const view = new VisualizationSlotView(requestId, inner)
    if (view.isTerminal()) {
      console.warn(
        `Visualizations.recordFailure: dropping failure for terminal slot ${requestId} ` +
          `(status=${view.status})`,
      )
      return
    }
    this.doc.transact(() => {
      inner.set(VIS_SLOT_FIELDS.failure, failure)
      inner.set(VIS_SLOT_FIELDS.status, 'failed' satisfies VisSlotStatus)
    })
  }
}
