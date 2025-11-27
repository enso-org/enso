import type { ConnectedEdge } from '$/providers/openedProjects/graph/graph'
import type { PortId } from '@/providers/portInfo'
import { Vec2 } from '@/util/data/vec2'
import * as iter from 'enso-common/src/utilities/data/iter'
import { computed, onBeforeUnmount, reactive, ref, watch, type WatchSource } from 'vue'
import type { AstId } from 'ydoc-shared/ast'

export type UnconnectedEdgeAnchor = { type: 'mouse' } | { type: 'fixed'; scenePos: Vec2 }

interface AnyUnconnectedEdge {
  source: AstId | undefined
  target: PortId | undefined
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: PortId
  /** Identifies what the disconnected end should be attached to. */
  anchor: UnconnectedEdgeAnchor
}
export interface UnconnectedSource extends AnyUnconnectedEdge {
  source: undefined
  target: PortId
}
export interface UnconnectedTarget extends AnyUnconnectedEdge {
  source: AstId
  target: undefined
  /** If true, the edge will be rendered in its dimmed color. */
  suggestion?: boolean
}
export type UnconnectedEdge = UnconnectedSource | UnconnectedTarget

export interface MouseEditedEdge {
  createdFrom: 'edge' | 'port' | 'newNodeButton'
  /** Position where the edge interaction started (not necessarily the source position). */
  startPosition: Vec2
}

/** TODO: Add docs */
export function useUnconnectedEdges() {
  const mouseEditedEdge = ref<UnconnectedEdge & MouseEditedEdge>()
  const cbEditedEdge = ref<UnconnectedTarget>()
  const outputSuggestedEdge = ref<UnconnectedTarget>()

  // === Mouse-edited edges ===

  function createEdgeFromOutput(source: AstId, event: PointerEvent) {
    mouseEditedEdge.value = {
      source,
      target: undefined,
      createdFrom: 'port',
      startPosition: new Vec2(event.screenX, event.screenY),
      anchor: { type: 'mouse' },
    }
  }

  function createEdgeFromPort(target: PortId, event: PointerEvent | undefined) {
    mouseEditedEdge.value = {
      source: undefined,
      target,
      createdFrom: 'port',
      startPosition: new Vec2(event?.screenX ?? 0, event?.screenY ?? 0),
      anchor: { type: 'mouse' },
    }
  }

  function createEdgeFromNewButton(source: AstId) {
    mouseEditedEdge.value = {
      source,
      target: undefined,
      createdFrom: 'newNodeButton',
      startPosition: Vec2.Zero,
      anchor: { type: 'mouse' },
    }
  }

  function disconnectSource(edge: ConnectedEdge, event: PointerEvent) {
    mouseEditedEdge.value = {
      source: undefined,
      target: edge.target,
      disconnectedEdgeTarget: edge.target,
      createdFrom: 'edge',
      startPosition: new Vec2(event.screenX, event.screenY),
      anchor: { type: 'mouse' },
    }
  }

  function disconnectTarget(edge: ConnectedEdge, event: PointerEvent) {
    mouseEditedEdge.value = {
      source: edge.source,
      target: undefined,
      disconnectedEdgeTarget: edge.target,
      createdFrom: 'edge',
      startPosition: new Vec2(event.screenX, event.screenY),
      anchor: { type: 'mouse' },
    }
  }

  // === Output-suggested edges ===

  function startOutputSuggestedEdge(portId: AstId) {
    outputSuggestedEdge.value = {
      source: portId,
      target: undefined,
      anchor: { type: 'mouse' },
      suggestion: true,
    }
    const createdEdge = outputSuggestedEdge.value
    return {
      endOutputSuggestedEdge: () => {
        if (outputSuggestedEdge.value === createdEdge) outputSuggestedEdge.value = undefined
      },
    }
  }

  function suggestEdgeFromOutput(portId: WatchSource<AstId | undefined>) {
    watch(portId, (portId, _prevPortId, onCleanup) => {
      if (portId) {
        const { endOutputSuggestedEdge } = startOutputSuggestedEdge(portId)
        onCleanup(endOutputSuggestedEdge)
      }
    })
  }

  const createNodeFromOutputPortButtonEdgesHovered = reactive(new Set<AstId>())
  const createNodeFromOutputPortButtonEdgePositions = reactive(new Map<AstId, Vec2>())
  function showCreateNodeButtonEdge(
    portId: AstId,
    { position, hovered }: { position: WatchSource<Vec2>; hovered: WatchSource<boolean> },
  ) {
    watch(
      position,
      (position) => createNodeFromOutputPortButtonEdgePositions.set(portId, position),
      { immediate: true },
    )
    // NOTE: Cleanup is *before* unmount for HMR to work correctly. When the module registering the
    // edge is hot-reloaded, if cleanup were in an unmount hook, the old version of the module would
    // delete the edge after the new version of the module initializes and registers it.
    onBeforeUnmount(() => createNodeFromOutputPortButtonEdgePositions.delete(portId))
    watch(
      hovered,
      (hovered) => {
        if (hovered) createNodeFromOutputPortButtonEdgesHovered.add(portId)
        else createNodeFromOutputPortButtonEdgesHovered.delete(portId)
      },
      { immediate: true },
    )
    onBeforeUnmount(() => createNodeFromOutputPortButtonEdgesHovered.delete(portId))
  }
  const createNodeFromOutputPortButtonEdges = computed<UnconnectedTarget[]>(() =>
    [...createNodeFromOutputPortButtonEdgePositions.entries()].map(([portId, position]) => ({
      source: portId,
      target: undefined,
      anchor: { type: 'fixed', scenePos: position },
      suggestion: !createNodeFromOutputPortButtonEdgesHovered.has(portId),
    })),
  )

  // === Edge status ===

  const unconnectedEdges = computed<Set<UnconnectedEdge>>(
    () =>
      new Set(
        iter.filterDefined([mouseEditedEdge.value, cbEditedEdge.value, outputSuggestedEdge.value]),
      ),
  )

  const unconnectedEdgeSources = computed(() => {
    const ports = new Set<AstId>()
    for (const edge of unconnectedEdges.value) {
      if (edge.source) ports.add(edge.source)
    }
    return ports
  })

  const disconnectedEdgeTargets = computed(() => {
    const ports = new Set<PortId>()
    for (const edge of unconnectedEdges.value) {
      if (edge.disconnectedEdgeTarget) ports.add(edge.disconnectedEdgeTarget)
    }
    return ports
  })

  function isDisconnected(edge: ConnectedEdge): boolean {
    return disconnectedEdgeTargets.value.has(edge.target)
  }

  return {
    // === Special edges ===
    mouseEditedEdge,
    cbEditedEdge,
    outputSuggestedEdge,
    createNodeFromOutputPortButtonEdges,
    showCreateNodeButtonEdge,
    // === Edge creation ===
    createEdgeFromPort,
    createEdgeFromOutput,
    createEdgeFromNewButton,
    disconnectSource,
    disconnectTarget,
    suggestEdgeFromOutput,
    // === Edge status ===
    isDisconnected,
    unconnectedEdgeSources,
  }
}
