import { describe, expect, it } from 'vitest'
import { YjsChannel } from 'ydoc-channel'
import type { ContextId, ExpressionId } from 'ydoc-shared/languageServerTypes'
import {
  newVisRequestId,
  Visualizations,
  type VisRequestId,
  type VisRequestPreprocessor,
  type VisualizationId,
} from 'ydoc-shared/visualizations'
import * as Y from 'yjs'
import {
  uuidToBytes,
  VIS_CONTROL_CHANNEL,
  VIS_DATA_CHANNEL,
  VisualizationBridge,
} from '../visualizationBridge'

// Polyfill for Node test environment.
if (typeof globalThis.CloseEvent === 'undefined') {
  class CloseEventPolyfill extends Event {
    constructor(type: string) {
      super(type)
    }
  }
  ;(globalThis as any).CloseEvent = CloseEventPolyfill
}

interface Fixture {
  indexDoc: Y.Doc
  visDoc: Y.Doc
  vis: Visualizations
  /**
   * Channel pair wired together through the index doc. The bridge holds one
   * end; the "LS peer" end is exposed here for assertions.
   */
  bridgeControl: YjsChannel<string>
  bridgeData: YjsChannel<Uint8Array>
  peerControl: YjsChannel<string>
  peerData: YjsChannel<Uint8Array>
  bridge: VisualizationBridge
  /** Messages seen by the peer as if it were the Language Server. */
  controlFromBridge: string[]
}

function makeFixture(): Fixture {
  const indexDoc = new Y.Doc()
  const visDoc = new Y.Doc()
  const vis = new Visualizations(visDoc)

  const bridgeControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const bridgeData = new YjsChannel<Uint8Array>(indexDoc, VIS_DATA_CHANNEL)
  const peerControl = new YjsChannel<string>(indexDoc, VIS_CONTROL_CHANNEL)
  const peerData = new YjsChannel<Uint8Array>(indexDoc, VIS_DATA_CHANNEL)

  const controlFromBridge: string[] = []
  peerControl.subscribe((msg) => controlFromBridge.push(msg))

  const bridge = new VisualizationBridge(vis, bridgeControl, bridgeData)

  return {
    indexDoc,
    visDoc,
    vis,
    bridgeControl,
    bridgeData,
    peerControl,
    peerData,
    bridge,
    controlFromBridge,
  }
}

function request(module = 'Standard.Visualization.Preprocessor'): VisRequestPreprocessor {
  return {
    visualizationModule: module,
    expression: 'identity',
  }
}

const VIS_ID = 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa' as VisualizationId
const CTX_ID = 'bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb' as ContextId
const NODE_ID = 'cccccccc-cccc-cccc-cccc-cccccccccccc' as ExpressionId

describe('VisualizationBridge', () => {
  it('emits an attach message when a pending slot appears', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      requestId,
    )
    expect(f.controlFromBridge).toHaveLength(1)
    const parsed = JSON.parse(f.controlFromBridge[0]!)
    expect(parsed.kind).toBe('attach')
    expect(parsed.requestId).toBe(requestId)
    expect(parsed.visualizationId).toBe(VIS_ID)
    expect(parsed.contextId).toBe(CTX_ID)
    expect(parsed.nodeExternalId).toBe(NODE_ID)
    expect(parsed.request.visualizationModule).toBe('Standard.Visualization.Preprocessor')
  })

  it('emits a detach message when a slot is removed', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      requestId,
    )
    f.vis.removeSlot(requestId)
    // One attach + one detach.
    expect(f.controlFromBridge).toHaveLength(2)
    const second = JSON.parse(f.controlFromBridge[1]!)
    expect(second.kind).toBe('detach')
    expect(second.requestId).toBe(requestId)
    // The bridge must carry the real visualizationId and contextId in detach
    // messages. Empty strings here would make the LS actor unable to resolve
    // the detach back to a runtime visualization, so the runtime would keep
    // producing updates after the client turned the visualization off.
    expect(second.visualizationId).toBe(VIS_ID)
    expect(second.contextId).toBe(CTX_ID)
  })

  it('writes response bytes into the matching slot when a data frame arrives', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      requestId,
    )

    const payload = new TextEncoder().encode('{"hello":"world"}')
    const frame = new Uint8Array(16 + payload.byteLength)
    frame.set(uuidToBytes(requestId), 0)
    frame.set(payload, 16)
    f.peerData.send(frame)

    const view = f.vis.getSlot(requestId as VisRequestId)
    expect(view?.status).toBe('ready')
    expect(view?.response).toBeInstanceOf(Uint8Array)
    expect(new TextDecoder().decode(view!.response!)).toBe('{"hello":"world"}')
  })

  it('records failure when a failed control message arrives', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      requestId,
    )
    f.peerControl.send(
      JSON.stringify({
        kind: 'failed',
        requestId,
        message: 'boom',
      }),
    )

    const view = f.vis.getSlot(requestId as VisRequestId)
    expect(view?.status).toBe('failed')
    expect(view?.failure?.message).toBe('boom')
  })

  it('emits an attach message with an inFrame expression when an in-frame slot appears', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: {
          visualizationModule: '',
          expression: { inFrame: '1 + 2' },
        },
      },
      requestId,
    )
    expect(f.controlFromBridge).toHaveLength(1)
    const parsed = JSON.parse(f.controlFromBridge[0]!)
    expect(parsed.kind).toBe('attach')
    expect(parsed.requestId).toBe(requestId)
    expect(parsed.contextId).toBe(CTX_ID)
    expect(parsed.nodeExternalId).toBe(NODE_ID)
    expect(parsed.request.expression).toEqual({ inFrame: '1 + 2' })
  })

  it('removes an in-frame slot after response without emitting detach', () => {
    const f = makeFixture()
    const requestId = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: {
          visualizationModule: '',
          expression: { inFrame: '1 + 2' },
        },
      },
      requestId,
    )

    const payload = new TextEncoder().encode('3')
    const frame = new Uint8Array(16 + payload.byteLength)
    frame.set(uuidToBytes(requestId), 0)
    frame.set(payload, 16)
    f.peerData.send(frame)

    const view = f.vis.getSlot(requestId as VisRequestId)
    expect(view?.status).toBe('ready')
    expect(new TextDecoder().decode(view!.response!)).toBe('3')

    // Simulate the client removing the slot once the one-shot response is read.
    f.vis.removeSlot(requestId)

    // In-frame one-shots are terminal on response: only the original `attach`
    // message should have been emitted on the control channel. Outright slot
    // removal must not produce a detach.
    const kinds = f.controlFromBridge.map((m) => JSON.parse(m).kind)
    expect(kinds).toEqual(['attach'])
  })

  it('supersede: new slot for same visualizationId triggers attach after old slot detach', () => {
    const f = makeFixture()
    const oldRequest = newVisRequestId()
    const newRequest = newVisRequestId()

    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      oldRequest,
    )
    // Simulate the client-side "modify": remove old slot, create new one.
    f.vis.removeSlot(oldRequest)
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request('Standard.Visualization.Other'),
      },
      newRequest,
    )

    // Bridge should have emitted: attach(old), detach(old), attach(new).
    const parsed = f.controlFromBridge.map((m) => JSON.parse(m))
    expect(parsed.map((m) => m.kind)).toEqual(['attach', 'detach', 'attach'])
    // The detach for the superseded slot must carry the real
    // visualizationId/contextId so the LS side can translate it into
    // Api.DetachVisualization rather than silently dropping it.
    expect(parsed[1]!.visualizationId).toBe(VIS_ID)
    expect(parsed[1]!.contextId).toBe(CTX_ID)
    expect(parsed[1]!.requestId).toBe(oldRequest)
    // Old slot is gone; new slot remains pending.
    expect(f.vis.getSlot(oldRequest as VisRequestId)).toBeNull()
    expect(f.vis.getSlot(newRequest as VisRequestId)?.status).toBe('pending')
  })

  it('prunes per-request state once a slot is removed', () => {
    const f = makeFixture()
    const rid = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      rid,
    )
    f.vis.removeSlot(rid)
    // Internal state check: the bridge should no longer track the rid.
    const internal = f.bridge as unknown as {
      announced: Set<string>
      oneshotRequestIds: Set<string>
      slotMeta: Map<string, unknown>
    }
    expect(internal.announced.has(rid)).toBe(false)
    expect(internal.oneshotRequestIds.has(rid)).toBe(false)
    expect(internal.slotMeta.has(rid)).toBe(false)
  })

  it('peer simulating the LS actor can resolve a detach back to the attach', () => {
    // This is a structural stand-in for `VisualizationBridgeActor`. It keys
    // its correlation map on visualizationId, exactly as the Scala side
    // does. If the bridge emits a detach with empty visualizationId, the
    // peer cannot clean up its tracking and cannot synthesize an
    // `Api.DetachVisualization`. This test pins that contract from the
    // ydoc-server side without having to reach the JVM.
    const f = makeFixture()
    const tracked = new Map<string, string>() // visualizationId -> requestId
    f.peerControl.subscribe((raw) => {
      const msg = JSON.parse(raw)
      if (msg.kind === 'attach') tracked.set(msg.visualizationId, msg.requestId)
      else if (msg.kind === 'detach') {
        if (!msg.visualizationId) return // simulates the early return
        tracked.delete(msg.visualizationId)
      }
    })

    const rid = newVisRequestId()
    f.vis.createSlot(
      {
        visualizationId: VIS_ID,
        contextId: CTX_ID,
        nodeExternalId: NODE_ID,
        request: request(),
      },
      rid,
    )
    expect(tracked.get(VIS_ID)).toBe(rid)

    f.vis.removeSlot(rid)
    // The peer must have been able to resolve the detach.
    expect(tracked.has(VIS_ID)).toBe(false)
  })
})
