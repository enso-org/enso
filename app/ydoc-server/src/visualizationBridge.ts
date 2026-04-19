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
  /** Request ids we have already emitted an `attach` for. */
  private readonly announced = new Set<string>()
  /** Request ids we have already emitted a `detach` for. */
  private readonly detached = new Set<string>()
  /** Request ids whose slot was an `inFrame` one-shot. We track these so
   *  that when the slot is subsequently removed from the map (client-side
   *  GC after reading the one-shot response) we know not to emit a detach
   *  for it. */
  private readonly oneshotRequestIds = new Set<string>()
  private readonly observer: () => void
  private readonly unsubscribeControl: () => void
  private readonly unsubscribeData: () => void
  private disposed = false

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

  /** Walks the slots map and emits attach / detach messages for state
   * changes. One-shot `inFrame` attaches never transition to `'detached'`
   * (the runtime auto-detaches internally) and are removed outright by the
   * client on `'ready' | 'failed'`; we skip both the detach-status emission
   * and the "outright removed" detach emission for those. */
  private scan(): void {
    const liveIds = new Set<string>()
    for (const view of this.vis.entries()) {
      const rid = view.requestId as unknown as string
      liveIds.add(rid)
      if (!this.announced.has(rid)) {
        if (view.status === 'pending') {
          this.announced.add(rid)
          if (isInFrameRequest(view.request)) this.oneshotRequestIds.add(rid)
          this.emitAttach(view)
        }
      }
      if (
        !this.oneshotRequestIds.has(rid) &&
        view.status === 'detached' &&
        !this.detached.has(rid)
      ) {
        this.detached.add(rid)
        this.emitDetach(view)
      }
    }
    // Emit detach for outright-removed slots we had previously announced —
    // but only if the slot's request wasn't an `inFrame` oneshot. For those,
    // removal on response is expected and carries no LS-side meaning.
    for (const rid of this.announced) {
      if (
        !liveIds.has(rid) &&
        !this.detached.has(rid) &&
        !this.oneshotRequestIds.has(rid)
      ) {
        this.detached.add(rid)
        this.emitRawDetach(rid)
      }
    }
  }

  private emitAttach(view: VisualizationSlotView): void {
    const visualizationId = view.visualizationId
    const contextId = view.contextId
    const nodeExternalId = view.nodeExternalId
    const request = view.request
    if (!visualizationId || !contextId || !nodeExternalId || !request) {
      console.warn('VisualizationBridge: slot missing required fields on attach', view.requestId)
      return
    }
    const msg: AttachMsg = {
      kind: 'attach',
      requestId: view.requestId,
      visualizationId,
      contextId,
      nodeExternalId,
      request,
    }
    this.control.send(JSON.stringify(msg))
  }

  private emitDetach(view: VisualizationSlotView): void {
    const visualizationId = view.visualizationId ?? ''
    const contextId = view.contextId ?? ''
    const msg: DetachMsg = {
      kind: 'detach',
      requestId: view.requestId,
      visualizationId,
      contextId,
    }
    this.control.send(JSON.stringify(msg))
  }

  /** Used when a slot was removed outright (no fields to read). */
  private emitRawDetach(requestId: string): void {
    const msg: DetachMsg = {
      kind: 'detach',
      requestId,
      visualizationId: '',
      contextId: '',
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
        // Data frame on vis:data carries the bytes; when it arrives we set
        // status to 'ready'. The `ready` control message is a redundant hint
        // kept for inspect visibility; we do not require it.
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
 *
 * The two endpoints of the data channel use **different codecs** on the same
 * `Y.Array`. The LS-side endpoint uses `JavaByteBufferCodec` to convert the
 * Java `ByteBuffer` that the Language Server sends into a `Uint8Array` for
 * storage; without this codec, `channel.send(byteBuffer)` stores the Java
 * object directly and Yjs serializes it as an empty `{}` on the wire. The
 * ydoc-server-side endpoint uses the identity codec because it reads and
 * writes the already-decoded `Uint8Array`. Pass `byteBufferClass` when
 * running under GraalJS polyglot; omit it in unit tests that only use
 * `Uint8Array` on both sides.
 */
export function createVisualizationBridge(
  indexDoc: Y.Doc,
  visSubdoc: Y.Doc,
  controlServer: YjsChannelServer<string>,
  dataServer: YjsChannelServer<JavaByteBuffer | Uint8Array>,
  byteBufferClass?: JavaByteBufferClass,
): VisualizationBridge {
  const bridgeControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const bridgeData = new YjsChannel<Uint8Array>(indexDoc, VIS_DATA_CHANNEL)
  const lsControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const lsData =
    byteBufferClass != null ?
      new YjsChannel<JavaByteBuffer, Uint8Array>(
        indexDoc,
        VIS_DATA_CHANNEL,
        new JavaByteBufferCodec(byteBufferClass),
      )
    : new YjsChannel<Uint8Array>(indexDoc, VIS_DATA_CHANNEL)
  controlServer.onConnect(lsControl)
  ;(dataServer as YjsChannelServer<unknown>).onConnect(lsData)
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

// Unused but kept for symmetry/tests: encode a UUID string into 16 bytes.
export function uuidToBytes(uuid: string): Uint8Array {
  const hex = uuid.replace(/-/g, '')
  if (hex.length !== 32) throw new Error(`Invalid UUID: ${uuid}`)
  const out = new Uint8Array(16)
  for (let i = 0; i < 16; i++) {
    out[i] = parseInt(hex.substring(i * 2, i * 2 + 2), 16)
  }
  return out
}
