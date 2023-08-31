<script setup lang="ts">
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import TopBar from '@/components/TopBar.vue'

import { useGraphStore, type ContentRange, type ExprId, type NodeId } from '@/stores/graph'
import type { Rect } from '@/stores/rect'
import { useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { ref } from 'vue'
import type { Breadcrumb } from './NavBreadcrumb.vue'

const viewportNode = ref<HTMLElement>()
const navigator = useNavigator(viewportNode)
const graphStore = useGraphStore()

const nodeRects = ref(new Map<NodeId, Rect>())
const exprRects = ref(new Map<ExprId, Rect>())

function updateNodeRect(id: NodeId, rect: Rect) {
  nodeRects.value.set(id, rect)
}

function updateExprRect(id: ExprId, rect: Rect) {
  exprRects.value.set(id, rect)
}

const circlePos = ref(Vec2.Zero())

function onViewportClick() {
  const pos = navigator.sceneMousePos
  if (pos != null) {
    circlePos.value = pos
  }
}

function keyboardBusy() {
  return document.activeElement != document.body
}

const breadcrumbs: Breadcrumb[] = [
  { text: 'main', onClick: () => { } },
  { text: 'ad_analytics', onClick: () => { } },
]

useWindowEvent('keypress', (e) => {
  if (keyboardBusy()) return
  const pos = navigator.sceneMousePos
  if (pos == null) return

  switch (e.key) {
    case 'n':
      const n = graphStore.createNode(pos)
      graphStore.setNodeContent(n, 'hello "world"! 123 + x')
      break
  }
})

function updateNodeContent(id: NodeId, range: ContentRange, content: string) {
  graphStore.replaceNodeSubexpression(id, range, content)
}
</script>

<template>
  <div ref="viewportNode" class="viewport" v-on="navigator.events" @click="onViewportClick">
    <svg :viewBox="navigator.viewBox">
      <circle :cx="circlePos.x" :cy="circlePos.y" r="6" fill="red" />
      <GraphEdge v-for="edge in graphStore.edges" :edge="edge" :nodeRects="nodeRects" :exprRects="exprRects" />
    </svg>
    <div :style="{ transform: navigator.transform }" class="htmlLayer">
      <div class="circle" :style="{ transform: `translate(${circlePos.x - 5}px, ${circlePos.y - 5}px)` }"></div>
      <GraphNode v-for="[id, node] in graphStore.nodes" :key="<any>id" :node="node"
        @updateNodeRect="updateNodeRect(id, $event)" @updateExprRect="updateExprRect"
        @updateNodeContent="(range, c) => updateNodeContent(id, range, c)" />
    </div>
    <TopBar :breadcrumbs="breadcrumbs" />
    <ComponentBrowser :navigator="navigator" />
  </div>
</template>

<style scoped>
.viewport {
  position: relative;
  contain: layout;
  overflow: hidden;
}

svg {
  position: absolute;
  top: 0;
  left: 0;
}

.htmlLayer {
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 0;
}

.circle {
  position: absolute;
  width: 10px;
  height: 10px;
  border-radius: 5px;
  background-color: purple;
}
</style>
