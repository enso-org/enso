<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import { useGraphStore, type Edge } from '@/stores/graph'
import { Vec2 } from '@/util/vec2.ts'
import type { ExprId } from 'shared/yjsModel.ts'
const graphStore = useGraphStore()

const props = defineProps<{
  sceneMousePos: Vec2 | null
  hoveredNode: ExprId | undefined
  hoveredExpr: ExprId | undefined
}>()
</script>

<template>
  <GraphEdge
    v-for="(edge, index) in graphStore.edges"
    :key="index"
    :edge="edge"
    :nodeRects="graphStore.nodeRects"
    :exprRects="graphStore.exprRects"
    :exprNodes="graphStore.exprNodes"
    :hoveredNode="hoveredNode"
    :hoveredExpr="hoveredExpr"
    :sceneMousePos="sceneMousePos"
    @disconnectSource="graphStore.disconnectSource(edge)"
    @disconnectTarget="graphStore.disconnectTarget(edge)"
  />
</template>
