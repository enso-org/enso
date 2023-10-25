<script setup lang="ts">
import GraphEdge from '@/components/GraphEditor/GraphEdge.vue'
import { useGraphStore } from '@/stores/graph'
const graphStore = useGraphStore()
</script>

<template>
  <GraphEdge
    v-for="(edge, index) in graphStore.edges"
    :key="index"
    :edge="edge"
    :nodeRects="graphStore.nodeRects"
    :exprRects="graphStore.exprRects"
    :exprNodes="graphStore.exprNodes"
  />
</template>
