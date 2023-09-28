<script setup lang="ts">
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import TopBar from '@/components/TopBar.vue'

import { useGraphStore } from '@/stores/graph'
import { ExecutionContext, useProjectStore } from '@/stores/project'
import type { Rect } from '@/stores/rect'
import { modKey, useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import { clamp } from '@vueuse/core'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { computed, onMounted, onUnmounted, reactive, ref, shallowRef, type Ref } from 'vue'

const EXECUTION_MODES = ['design', 'live']

const title = ref('Test Project')
const mode = ref('design')
const viewportNode = ref<HTMLElement>()
const navigator = useNavigator(viewportNode)
const graphStore = useGraphStore()
const projectStore = useProjectStore()
const executionCtx = shallowRef<ExecutionContext>()
const componentBrowserVisible = ref(false)
const componentBrowserPosition = ref(Vec2.Zero())

const nodeRects = reactive(new Map<ExprId, Rect>())
const exprRects = reactive(new Map<ExprId, Rect>())

onMounted(async () => {
  const executionCtxPromise = projectStore.createExecutionContextForMain()
  onUnmounted(async () => {
    executionCtx.value = undefined
    const ctx = await executionCtxPromise
    if (ctx != null) ctx.destroy()
  })
  executionCtx.value = (await executionCtxPromise) ?? undefined
})

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

interface Interaction {
  cancel?(): void
  handleExprClick?(expr: ExprId): void
  handleNodeClick?(node: ExprId): void
}
let currentInteraction: Interaction | null = null
function setCurrentInteraction(interaction: Interaction | null) {
  if (currentInteraction !== null) {
    if (currentInteraction.cancel !== undefined) {
      currentInteraction.cancel()
    }
  }
  currentInteraction = interaction
}

useWindowEvent('keydown', (e) => {
  if (keyboardBusy()) return
  const pos = navigator.sceneMousePos

  if (modKey(e)) {
    switch (e.key) {
      case 'z':
        projectStore.undoManager.undo()
        break
      case 'y':
        projectStore.undoManager.redo()
        break
    }
  } else {
    switch (e.key) {
      case 'Enter':
        if (pos != null && !componentBrowserVisible.value) {
          componentBrowserPosition.value = pos
          componentBrowserVisible.value = true
        }
        break
      case 'Escape':
        setCurrentInteraction(null)
        break
      case 'n': {
        if (pos != null) graphStore.createNode(pos, 'hello "world"! 123 + x')
        break
      }
    }
  }
})

function updateNodeContent(id: ExprId, range: ContentRange, content: string) {
  graphStore.replaceNodeSubexpression(id, range, content)
}

function moveNode(id: ExprId, delta: Vec2) {
  const node = graphStore.nodes.get(id)
  if (node == null) return
  const newPosition = node.position.addScaled(delta, 1 / navigator.scale)
  graphStore.setNodePosition(id, newPosition)
}

function layoutEdge(edge: {
  source?: ExprId | null
  target?: ExprId | null
}): { source: Vec2; target: Vec2 } | null {
  let target
  if (edge.target != null) {
    const targetNodeId = graphStore.exprNodes.get(edge.target)
    if (targetNodeId == null) return null
    let targetNodeRect = nodeRects.get(targetNodeId)
    let targetRect = exprRects.get(edge.target)
    if (targetRect == null || targetNodeRect == null) return null
    target = targetRect.center().add(targetNodeRect.pos)
  } else {
    target = circlePos.value
  }
  let source
  if (edge.source != null) {
    let sourceNodeRect = nodeRects.get(edge.source)
    if (sourceNodeRect == null) return null
    const sourcePos = sourceNodeRect.center()
    const sourceRangeX = sourceNodeRect.rangeX()
    const EDGE_PADDING = 20
    const x = clamp(target.x, sourceRangeX[0] + EDGE_PADDING, sourceRangeX[1] - EDGE_PADDING)
    source = sourcePos.withX(x)
  } else {
    source = circlePos.value
  }
  return { source, target }
}

const connectedEdges = computed(() => {
  const edges = []
  for (const edge of graphStore.edges) {
    const layout = layoutEdge(edge)
    if (layout) edges.push(layout)
  }
  return edges
})
function edgeTarget(expr: ExprId | null): ExprId | null {
  if (expr === null) return null
  // TODO: Check if the hovered expr is a legal target; if not, recursively try its ancestors.
  return expr
}
const unconnectedEdges = computed(() => {
  const unconnected = graphStore.unconnectedEdge
  if (unconnected === null) return []
  const edge = layoutEdge({
    source: unconnected.source ?? hoveredNode.value,
    target: unconnected.target ?? edgeTarget(hoveredExpr.value),
  })
  if (edge === null) return []
  return [edge]
})
function disconnect(edgeIndex: number, disconnectEnd: 'source' | 'target') {
  const edge = graphStore.edges[edgeIndex]!
  if (disconnectEnd === 'source') {
    graphStore.unconnectedEdge = {
      target: edge.target,
      disconnectedEdgeTarget: edge.target,
    }
  } else if (disconnectEnd === 'target') {
    graphStore.unconnectedEdge = {
      source: edge.source,
      disconnectedEdgeTarget: edge.target,
    }
  }
  setCurrentInteraction({
    cancel: () => {
      graphStore.unconnectedEdge = null
    },
  })
}

const hoveredNode: Ref<ExprId | null> = ref(null)
const hoveredExpr: Ref<ExprId | null> = ref(null)
function updateHoveredExpr(expr: ExprId | null) {
  hoveredExpr.value = expr
}
</script>

<template>
  <div ref="viewportNode" class="viewport" v-on="navigator.events" @mousemove="updateMousePos">
    <svg :viewBox="navigator.viewBox">
      <GraphEdge
        v-for="(edge, index) in connectedEdges"
        :key="index"
        :edge="edge"
        :navigator="navigator"
        :editing="false"
        @disconnectSource="disconnect(index, 'source')"
        @disconnectTarget="disconnect(index, 'target')"
      />
      <GraphEdge
        v-for="edge in unconnectedEdges"
        :edge="edge"
        :navigator="navigator"
        :editing="true"
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
        @movePosition="moveNode(id, $event)"
        @mouseenter="hoveredNode = id"
        @mouseleave="hoveredNode = null"
        @updateHoveredExpr="updateHoveredExpr"
      />
    </div>
    <ComponentBrowser
      v-if="componentBrowserVisible"
      :navigator="navigator"
      :position="componentBrowserPosition"
      @finished="componentBrowserVisible = false"
    />
    <TopBar
      v-model:mode="mode"
      :title="title"
      :modes="EXECUTION_MODES"
      :breadcrumbs="['main', 'ad_analytics']"
      @breadcrumbClick="console.log(`breadcrumb #${$event + 1} clicked.`)"
      @back="console.log('breadcrumbs \'back\' button clicked.')"
      @forward="console.log('breadcrumbs \'forward\' button clicked.')"
      @execute="console.log('\'execute\' button clicked.')"
    />
    <CodeEditor ref="codeEditor" />
  </div>
</template>

<style scoped>
.viewport {
  position: relative;
  contain: layout;
  overflow: clip;
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
