import type { GraphStore, Node } from '$/providers/openedProjects/graph'
import { nodeId } from '$/providers/openedProjects/graph/graphDatabase'
import type { ModuleStore } from '$/providers/openedProjects/module/module'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import type { Ref } from 'vue'

/**
 * Create handlers for aligning selected nodes in the graph.
 * @param graphStore - The graph store containing node information
 * @param module - A reference to the module store for batch editing
 * @returns An object containing alignment handler functions
 */
export function createSelectionAlignmentHandlers(graphStore: GraphStore, module: Ref<ModuleStore>) {
  function alignLeftNodes(nodes: Node[]) {
    const alignable = nodes.filter((node) => Number.isFinite(node.position.x))
    if (alignable.length === 0) return
    const leftMostX = Math.min(...alignable.map((node) => node.position.x))
    if (!Number.isFinite(leftMostX)) return
    batchUpdatePositions(
      alignable.map((node) => ({
        node,
        position: new Vec2(leftMostX, node.position.y),
      })),
    )
  }

  function alignRightNodes(nodes: Node[]) {
    const rects = getNodesWithRects(nodes)
    if (rects.length === 0) return
    const rightMostX = Math.max(...rects.map(({ node, rect }) => node.position.x + rect.width))
    if (!Number.isFinite(rightMostX)) return
    batchUpdatePositions(
      rects.map(({ node, rect }) => ({
        node,
        position: new Vec2(rightMostX - rect.width, node.position.y),
      })),
    )
  }

  function alignCenterNodes(nodes: Node[]) {
    const rects = getNodesWithRects(nodes)
    if (rects.length === 0) return
    const centerX =
      rects.reduce((sum, { rect }) => sum + rect.left + rect.width / 2, 0) / rects.length
    if (!Number.isFinite(centerX)) return
    batchUpdatePositions(
      rects.map(({ node, rect }) => ({
        node,
        position: new Vec2(centerX - rect.width / 2, node.position.y),
      })),
    )
  }

  function alignTopNodes(nodes: Node[]) {
    const alignable = nodes.filter((node) => Number.isFinite(node.position.y))
    if (alignable.length === 0) return
    const topMostY = Math.min(...alignable.map((node) => node.position.y))
    if (!Number.isFinite(topMostY)) return
    batchUpdatePositions(
      alignable.map((node) => ({
        node,
        position: new Vec2(node.position.x, topMostY),
      })),
    )
  }

  function alignBottomNodes(nodes: Node[]) {
    const rects = getNodesWithRects(nodes)
    if (rects.length === 0) return
    const bottomMostY = Math.max(...rects.map(({ rect }) => rect.bottom))
    if (!Number.isFinite(bottomMostY)) return
    batchUpdatePositions(
      rects.map(({ node, rect }) => ({
        node,
        position: new Vec2(node.position.x, bottomMostY - rect.height),
      })),
    )
  }

  function getNodesWithRects(nodes: Node[]) {
    return nodes
      .map((node) => {
        const rect = graphStore.visibleArea(nodeId(node))
        return rect ? { node, rect } : null
      })
      .filter((entry): entry is { node: Node; rect: Rect } => entry != null)
  }

  function batchUpdatePositions(updates: Array<{ node: Node; position: Vec2 }>) {
    if (updates.length === 0) return
    module.value.batchEdits(() => {
      for (const { node, position } of updates) {
        graphStore.setNodePosition(nodeId(node), position)
      }
    })
  }

  return {
    alignLeftNodes,
    alignCenterNodes,
    alignRightNodes,
    alignTopNodes,
    alignBottomNodes,
  }
}
