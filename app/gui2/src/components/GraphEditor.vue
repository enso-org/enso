<script setup lang="ts">
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import { useGraphStore } from '@/stores/graph'
import type { Rect } from '@/stores/rect'
import { useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from '../../shared/yjs-model'
import { reactive, ref } from 'vue'

const viewportNode = ref<HTMLElement>()
const navigator = useNavigator(viewportNode)
const graphStore = useGraphStore()

const nodeRects = reactive(new Map<ExprId, Rect>())
const exprRects = reactive(new Map<ExprId, Rect>())

function updateNodeRect(id: ExprId, rect: Rect) {
  nodeRects.set(id, rect)
}

function updateExprRect(id: ExprId, rect: Rect) {
  exprRects.set(id, rect)
}

const circlePos = ref(Vec2.Zero())

function updateMousePos() {
  const pos = navigator.sceneMousePos
  if (pos != null) {
    circlePos.value = pos
  }
}

function keyboardBusy() {
  return document.activeElement != document.body
}

useWindowEvent('keypress', (e) => {
  if (keyboardBusy()) return
  const pos = navigator.sceneMousePos
  if (pos == null) return

  switch (e.key) {
    case 'n':
      const n = graphStore.createNode(pos)
      if (n == null) return
      graphStore.setNodeContent(n, 'hello "world"! 123 + x')
      break
  }
})

function updateNodeContent(id: ExprId, range: ContentRange, content: string) {
  graphStore.replaceNodeSubexpression(id, range, content)
}
</script>

<template>
  <div ref="viewportNode" class="viewport" v-on="navigator.events" @mousemove="updateMousePos">
    <svg :viewBox="navigator.viewBox">
      <GraphEdge
        v-for="edge in graphStore.edges"
        :edge="edge"
        :nodeRects="nodeRects"
        :exprRects="exprRects"
        :exprNodes="graphStore.exprNodes"
      />
    </svg>
    <div :style="{ transform: navigator.transform }" class="htmlLayer">
      <GraphNode
        v-for="[id, node] in graphStore.nodes"
        :key="id"
        :node="node"
        @updateRect="updateNodeRect(id, $event)"
        @delete="graphStore.deleteNode(id)"
        @updateExprRect="updateExprRect"
        @updateContent="(range, c) => updateNodeContent(id, range, c)"
        @updatePosition="graphStore.setNodePosition(id, $event)"
      />
    </div>
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
