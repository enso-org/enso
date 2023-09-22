<script setup lang="ts">
import type { Edge } from '@/stores/graph'
import type { Rect } from '@/stores/rect'
import { clamp } from '@vueuse/core'
import type { ExprId } from 'shared/yjsModel'
import { computed } from 'vue'

const props = defineProps<{
  edge: Edge
  nodeRects: Map<ExprId, Rect>
  exprRects: Map<ExprId, Rect>
  exprNodes: Map<ExprId, ExprId>
}>()

const edgePath = computed(() => {
  let edge = props.edge
  const targetNodeId = props.exprNodes.get(edge.target)
  if (targetNodeId == null) return
  let sourceNodeRect = props.nodeRects.get(edge.source)
  let targetNodeRect = props.nodeRects.get(targetNodeId)
  let targetRect = props.exprRects.get(edge.target)
  if (sourceNodeRect == null || targetRect == null || targetNodeRect == null) return
  let sourcePos = sourceNodeRect.center()
  let targetPos = targetRect.center().add(targetNodeRect.pos)

  let sourceRangeX = sourceNodeRect.rangeX()
  const EDGE_PADDING = 20
  const sourceX = clamp(targetPos.x, sourceRangeX[0] + EDGE_PADDING, sourceRangeX[1] - EDGE_PADDING)
  const LINE_OUT = 20
  const QUAD_OUT = 50

  const midpointX = (sourceX + targetPos.x) / 2
  const midpointY = (sourcePos.y + targetPos.y) / 2

  return `
    M ${sourceX} ${sourcePos.y}
    L ${sourceX} ${sourcePos.y + LINE_OUT}
    Q ${sourceX} ${sourcePos.y + QUAD_OUT} ${midpointX} ${midpointY}
    Q ${targetPos.x} ${targetPos.y - QUAD_OUT} ${targetPos.x} ${targetPos.y - LINE_OUT}
    L ${targetPos.x} ${targetPos.y}
  `
})
</script>

<template>
  <path :d="edgePath" stroke="black" stroke-width="4" fill="none" class="edge" />
</template>

<style scoped>
.edge {
  stroke: tan;
}

.edge:hover {
  stroke: red;
}
</style>
