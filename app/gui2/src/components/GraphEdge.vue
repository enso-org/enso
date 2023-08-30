<script setup lang="ts">
import type { Edge, ExprId, NodeId } from '@/stores/graph'
import type { Rect } from '@/stores/rect'
import { computed } from 'vue'

const props = defineProps<{
  edge: Edge
  nodeRects: Map<NodeId, Rect>
  exprRects: Map<ExprId, Rect>
}>()

const edgePath = computed(() => {
  let edge = props.edge
  let sourceRect = props.nodeRects.get(edge.source)
  let targetNode = (edge.target >> 64n) as NodeId
  let targetNodeRect = props.nodeRects.get(targetNode)
  let targetRect = props.exprRects.get(edge.target)
  if (sourceRect == null || targetRect == null || targetNodeRect == null) return ''
  let sourcePos = sourceRect.center()
  let targetPos = targetRect.center().add(targetNodeRect.pos)
  return `M ${sourcePos.x} ${sourcePos.y} L ${targetPos.x} ${targetPos.y}`
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
