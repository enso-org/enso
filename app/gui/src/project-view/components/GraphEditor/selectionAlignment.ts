import type { GraphStore, Node } from '$/providers/openedProjects/graph'
import type { ModuleStore } from '$/providers/openedProjects/module/module'
import { nodeId } from '$/providers/openedProjects/graph/graphDatabase'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import type { Ref } from 'vue'

export function createSelectionAlignmentHandlers(
  graphStore: GraphStore,
  module: Ref<ModuleStore>,
) {
  function alignLeftNodes(nodes: Node[]) {
    const alignable = nodes.filter((node) => Number.isFinite(node.position.x))
    if (alignable.length === 0) return
    const leftMostX = Math.min(...alignable.map((node) => node.position.x))
    if (!Number.isFinite(leftMostX)) return
    module.value.batchEdits(() => {
      for (const node of alignable) {
        graphStore.setNodePosition(nodeId(node), new Vec2(leftMostX, node.position.y))
      }
    })
  }

  function alignRightNodes(nodes: Node[]) {
    const rects = nodes
      .map((node) => ({
        node,
        rect: graphStore.nodeRects.get(nodeId(node)),
      }))
      .filter((entry): entry is { node: Node; rect: Rect } => entry.rect != null)
    if (rects.length === 0) return
    const rightMostX = Math.max(...rects.map(({ node, rect }) => node.position.x + rect.size.x))
    if (!Number.isFinite(rightMostX)) return
    module.value.batchEdits(() => {
      for (const { node, rect } of rects) {
        graphStore.setNodePosition(
          nodeId(node),
          new Vec2(rightMostX - rect.size.x, node.position.y),
        )
      }
    })
  }

  function alignCenterNodes(nodes: Node[]) {
    const rects = nodes
      .map((node) => {
        const rect = graphStore.visibleArea(nodeId(node))
        return rect ? { node, rect } : null
      })
      .filter((entry): entry is { node: Node; rect: Rect } => entry != null)
    if (rects.length === 0) return
    const centerX =
      rects.reduce((sum, { rect }) => sum + rect.left + rect.width / 2, 0) / rects.length
    if (!Number.isFinite(centerX)) return
    module.value.batchEdits(() => {
      for (const { node, rect } of rects) {
        graphStore.setNodePosition(
          nodeId(node),
          new Vec2(centerX - rect.width / 2, node.position.y),
        )
      }
    })
  }

  function alignTopNodes(nodes: Node[]) {
    const alignable = nodes.filter((node) => Number.isFinite(node.position.y))
    if (alignable.length === 0) return
    const topMostY = Math.min(...alignable.map((node) => node.position.y))
    if (!Number.isFinite(topMostY)) return
    module.value.batchEdits(() => {
      for (const node of alignable) {
        graphStore.setNodePosition(nodeId(node), new Vec2(node.position.x, topMostY))
      }
    })
  }

  function alignBottomNodes(nodes: Node[]) {
    const rects = nodes
      .map((node) => {
        const rect = graphStore.visibleArea(nodeId(node))
        return rect ? { node, rect } : null
      })
      .filter((entry): entry is { node: Node; rect: Rect } => entry != null)
    if (rects.length === 0) return
    const bottomMostY = Math.max(...rects.map(({ rect }) => rect.bottom))
    if (!Number.isFinite(bottomMostY)) return
    module.value.batchEdits(() => {
      for (const { node, rect } of rects) {
        graphStore.setNodePosition(
          nodeId(node),
          new Vec2(node.position.x, bottomMostY - rect.height),
        )
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
