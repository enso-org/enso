import type { PortId } from '@/providers/portInfo'
import type { ConnectedEdge } from '@/stores/graph/index'
import { filterDefined } from '@/util/data/iterable'
import { Vec2 } from '@/util/data/vec2'
import { computed, ref, watch, type WatchSource } from 'vue'
import type { AstId } from 'ydoc-shared/ast'

export type UnconnectedEdgeAnchor = { type: 'mouse' } | { type: 'fixed'; scenePos: Vec2 }

interface AnyUnconnectedEdge {
  source: AstId | undefined
  target: PortId | undefined
  /** If this edge represents an in-progress edit of a connected edge, it is identified by its target expression. */
  disconnectedEdgeTarget?: PortId
  /** Identifies what the disconnected end should be attached to. */
  anchor: UnconnectedEdgeAnchor
  /** CSS value; if provided, overrides any color calculation. */
  color?: string
}
export interface UnconnectedSource extends AnyUnconnectedEdge {
  source: undefined
  target: PortId
}
export interface UnconnectedTarget extends AnyUnconnectedEdge {
  source: AstId
  target: undefined
  /** If true, the target end should be drawn as with a self-argument arrow. */
  targetIsSelfArgument?: boolean
  /** If true, the edge will be rendered in its dimmed color. */
  suggestion?: boolean
}
export type UnconnectedEdge = UnconnectedSource | UnconnectedTarget

export interface MouseEditedEdge {
  /** A pointer event which caused the unconnected edge */
  event: PointerEvent | undefined
}

/** TODO: Add docs */
export function useUnconnectedEdges() {
  const mouseEditedEdge = ref<UnconnectedEdge & MouseEditedEdge>()
  const cbEditedEdge = ref<UnconnectedTarget>()
  const outputSuggestedEdge = ref<UnconnectedTarget>()

  // === Mouse-edited edges ===

  function createEdgeFromOutput(source: AstId, event: PointerEvent | undefined) {
    mouseEditedEdge.value = { source, target: undefined, event, anchor: { type: 'mouse' } }
  }

  function disconnectSource(edge: ConnectedEdge, event: PointerEvent | undefined) {
    mouseEditedEdge.value = {
      source: undefined,
      target: edge.target,
      disconnectedEdgeTarget: edge.target,
      event,
      anchor: { type: 'mouse' },
    }
  }

  function disconnectTarget(edge: ConnectedEdge, event: PointerEvent | undefined) {
    mouseEditedEdge.value = {
      source: edge.source,
      target: undefined,
      disconnectedEdgeTarget: edge.target,
      event,
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

  // === Edge status ===

  const unconnectedEdges = computed<Set<UnconnectedEdge>>(
    () =>
      new Set(
        filterDefined([mouseEditedEdge.value, cbEditedEdge.value, outputSuggestedEdge.value]),
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
    // === Edge creation ===
    createEdgeFromOutput,
    disconnectSource,
    disconnectTarget,
    suggestEdgeFromOutput,
    // === Edge status ===
    isDisconnected,
    unconnectedEdgeSources,
  }
}
