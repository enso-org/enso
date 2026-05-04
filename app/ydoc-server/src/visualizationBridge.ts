/**
 * Ydoc-server side of the visualization transport.
 *
 * Observes the vis subdoc for slot mutations and forwards them to the
 * Language Server via a JSON control channel. Receives response bytes and
 * failure notifications from the LS and writes them back into the slot. The
 * Language Server never touches the Y.Doc directly - this module is the sole
 * bridge.
 */

import { YjsChannel, type YjsChannelServer } from 'ydoc-channel'
import type { Diagnostic } from 'ydoc-shared/languageServerTypes'
import {
  isInFrameRequest,
  Visualizations,
  VisualizationSlotView,
  type VisRequestId,
} from 'ydoc-shared/visualizations'
import type * as Y from 'yjs'
import {
  JavaByteBufferCodec,
  type JavaByteBuffer,
  type JavaByteBufferClass,
} from './YjsBinaryChannel'

const REQUEST_ID_BYTES = 16

/** The control-channel message envelope as exchanged as JSON strings. */
type ControlMsg = AttachMsg | DetachMsg | ReadyMsg | FailedMsg

interface AttachMsg {
  kind: 'attach'
  requestId: string
  visualizationId: string
  contextId: string
  nodeExternalId: string
  request: unknown
}
interface DetachMsg {
  kind: 'detach'
  requestId: string
  visualizationId: string
  contextId: string
}
interface ReadyMsg {
  kind: 'ready'
  requestId: string
}
interface FailedMsg {
  kind: 'failed'
  requestId: string
  message: string
  diagnostic?: Diagnostic
}

/**
 * Bridge between a vis subdoc and a pair of LS channels.
 *
 * One instance per `LanguageServerSession`. The bridge is constructed with
 * channels that have already been registered with the LS callback server via
 * `onConnect`, and with a `Visualizations` wrapper over the authoritative vis
 * subdoc.
 */
export class VisualizationBridge {
  private readonly vis: Visualizations
  private readonly control: YjsChannel<string>
  private readonly data: YjsChannel<Uint8Array>
  /**
   * Request ids we have observed at least once during `scan()`. Tracked on
   * first observation regardless of status so a slot that never passed
   * through `pending` (e.g. a `ready` slot written in the same transaction
   * it was created in, or an already-terminal slot restored from a prior
   * session's subdoc) still has a known "live" set. Entries are pruned when
   * the rid leaves the live set.
   */
  private readonly seenRids = new Set<string>()
  /**
   * Request ids whose slot was an `inFrame` one-shot. We track these so
   * that when the slot is subsequently removed from the map (client-side
   * GC after reading the one-shot response) we know not to emit a detach
   * for it. Pruned together with `seenRids`.
   */
  private readonly oneshotRequestIds = new Set<string>()
  /**
   * Identity fields captured at attach time for rids for which we actually
   * emitted an `attach` to the LS. The slot is gone from the Y.Map by the
   * time we observe its removal, so we cannot read these fields then.
   * Populating a detach message with the real `visualizationId` +
   * `contextId` is what lets the Language Server actor translate it into an
   * `Api.DetachVisualization`. Presence in this map also serves as the
   * "already-attached" signal so removals of rids we never attached (e.g.
   * a slot that was `failed` on first observation) do not fabricate a
   * detach message for state the LS never held.
   */
  private readonly slotMeta = new Map<string, { visualizationId: string; contextId: string }>()
  private readonly observer: () => void
  private readonly unsubscribeControl: () => void
  private readonly unsubscribeData: () => void
  private disposed = false

  /** Observe `vis` and translate slot mutations into `control` / `data` frames. */
  constructor(vis: Visualizations, control: YjsChannel<string>, data: YjsChannel<Uint8Array>) {
    this.vis = vis
    this.control = control
    this.data = data

    this.unsubscribeControl = this.control.subscribe((msg) => this.onControlMessage(msg))
    this.unsubscribeData = this.data.subscribe((buf) => this.onDataFrame(buf))

    this.observer = () => {
      if (!this.disposed) this.scan()
    }
    this.vis.slots.observeDeep(this.observer)
    // Emit attaches for any slots that were already present before we wired up.
    this.scan()
  }

  /**
   * Walk the slots map and reconcile known rids with the live set. For each
   * first-seen rid, emit an `attach` **only** if the slot is `pending`. A
   * slot that is already terminal on first observation has no runtime work to
   * kick off and is tracked passively so its eventual removal does not
   * fabricate a detach for state the LS never held. For each rid that left
   * the live set since the last scan, emit a `detach` iff we previously sent
   * an `attach` for it (`slotMeta` holds that record) and it is not a
   * one-shot `inFrame` request (those the runtime auto-detaches).
   */
  private scan(): void {
    const liveIds = new Set<string>()
    for (const view of this.vis.entries()) {
      const rid = view.requestId
      liveIds.add(rid)
      if (this.seenRids.has(rid)) continue
      this.seenRids.add(rid)
      if (view.status === 'pending') {
        if (this.emitAttach(view)) {
          if (isInFrameRequest(view.request)) this.oneshotRequestIds.add(rid)
        }
      } else {
        console.warn(
          `VisualizationBridge: first observed slot ${rid} with non-pending status ` +
            `${view.status ?? '<unset>'}; tracking passively without attach`,
        )
      }
    }
    for (const rid of Array.from(this.seenRids)) {
      if (liveIds.has(rid)) continue
      if (!this.oneshotRequestIds.has(rid)) {
        const meta = this.slotMeta.get(rid)
        if (meta) this.emitDetach(rid, meta)
      }
      this.seenRids.delete(rid)
      this.oneshotRequestIds.delete(rid)
      this.slotMeta.delete(rid)
    }
  }

  /**
   * Emit an attach for a newly-pending slot. Returns true on success, false
   * if the slot was malformed and the caller should skip tracking it.
   */
  private emitAttach(view: VisualizationSlotView): boolean {
    const visualizationId = view.visualizationId
    const contextId = view.contextId
    const nodeExternalId = view.nodeExternalId
    const request = view.request
    if (!visualizationId || !contextId || !nodeExternalId || !request) {
      console.warn('VisualizationBridge: slot missing required fields on attach', view.requestId)
      return false
    }
    this.slotMeta.set(view.requestId, { visualizationId, contextId })
    const msg: AttachMsg = {
      kind: 'attach',
      requestId: view.requestId,
      visualizationId,
      contextId,
      nodeExternalId,
      request,
    }
    this.control.send(JSON.stringify(msg))
    return true
  }

  private emitDetach(
    requestId: string,
    meta: { visualizationId: string; contextId: string },
  ): void {
    const msg: DetachMsg = {
      kind: 'detach',
      requestId,
      visualizationId: meta.visualizationId,
      contextId: meta.contextId,
    }
    this.control.send(JSON.stringify(msg))
  }

  private onControlMessage(raw: string): void {
    if (this.disposed) return
    let msg: ControlMsg
    try {
      msg = JSON.parse(raw) as ControlMsg
    } catch (e) {
      console.warn('VisualizationBridge: invalid control message JSON', e)
      return
    }
    const rid = msg.requestId as VisRequestId
    switch (msg.kind) {
      case 'failed': {
        const failure =
          msg.diagnostic != null ?
            { message: msg.message, diagnostic: msg.diagnostic }
          : { message: msg.message }
        this.vis.recordFailure(rid, failure)
        break
      }
      case 'ready':
        // Data frame on vis:data carries the bytes. When it arrives we set
        // status to 'ready'. The `ready` control message is a redundant hint
        // kept for inspect visibility as we do not require it.
        break
      case 'attach':
      case 'detach':
        // These are ydoc -> LS only. If the LS echoes them we ignore.
        break
      default: {
        const _exhaustive: never = msg
        void _exhaustive
      }
    }
  }

  private onDataFrame(frame: Uint8Array): void {
    if (this.disposed) return
    if (frame.byteLength < REQUEST_ID_BYTES) {
      console.warn('VisualizationBridge: data frame shorter than request id header')
      return
    }
    const ridBytes = frame.subarray(0, REQUEST_ID_BYTES)
    const payload = frame.subarray(REQUEST_ID_BYTES)
    const rid = uuidFromBytes(ridBytes) as VisRequestId
    // `subarray` shares the underlying buffer; copy so the slot owns its bytes.
    const owned = new Uint8Array(payload.byteLength)
    owned.set(payload)
    this.vis.recordResponse(rid, owned)
  }

  /** Unsubscribe from observers and channels. Idempotent. */
  close(): void {
    if (this.disposed) return
    this.disposed = true
    this.vis.slots.unobserveDeep(this.observer)
    this.unsubscribeControl()
    this.unsubscribeData()
  }
}

/**
 * Lazily create the pair of vis channels for a session and wire them to the
 * given LS callback servers. The channels live on the session's index doc.
 *
 * Creates **two** `YjsChannel` instances per channel name. One for the
 * ydoc-server-side bridge and a second fresh instance passed to the LS
 * callback. A `YjsChannel` filters out transactions with its own `senderId`
 * to prevent echoes, so the two endpoints must be different instances;
 * reusing one instance would cause messages sent by the bridge to be
 * echo-suppressed before the LS-side subscribe handler fires. This mirrors
 * how `YjsServerTransport` wires the JSON channel pair.
 */
export function createVisualizationBridge(
  indexDoc: Y.Doc,
  visSubdoc: Y.Doc,
  controlServer: YjsChannelServer<string>,
  dataServer: YjsChannelServer<JavaByteBuffer>,
  byteBufferClass: JavaByteBufferClass,
): VisualizationBridge {
  const bridgeControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const bridgeData = new YjsChannel<Uint8Array>(indexDoc, VIS_DATA_CHANNEL)
  const lsControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const lsData = new YjsChannel<JavaByteBuffer, Uint8Array>(
    indexDoc,
    VIS_DATA_CHANNEL,
    new JavaByteBufferCodec(byteBufferClass),
  )
  controlServer.onConnect(lsControl)
  dataServer.onConnect(lsData)
  return new VisualizationBridge(new Visualizations(visSubdoc), bridgeControl, bridgeData)
}

export const VIS_CONTROL_CHANNEL = 'vis:control'
export const VIS_DATA_CHANNEL = 'vis:data'

const HEX = '0123456789abcdef'

function uuidFromBytes(bytes: Uint8Array): string {
  if (bytes.byteLength !== 16) throw new Error(`Expected 16-byte UUID, got ${bytes.byteLength}`)
  let s = ''
  for (let i = 0; i < 16; i++) {
    const b = bytes[i]!
    s += HEX[b >>> 4]! + HEX[b & 0xf]!
    if (i === 3 || i === 5 || i === 7 || i === 9) s += '-'
  }
  return s
}

/** Encode a UUID string into its 16 raw bytes. Used by tests and symmetric with {@link uuidFromBytes}. */
export function uuidToBytes(uuid: string): Uint8Array {
  const hex = uuid.replace(/-/g, '')
  if (hex.length !== 32) throw new Error(`Invalid UUID: ${uuid}`)
  const out = new Uint8Array(16)
  for (let i = 0; i < 16; i++) {
    out[i] = parseInt(hex.substring(i * 2, i * 2 + 2), 16)
  }
  return out
}
