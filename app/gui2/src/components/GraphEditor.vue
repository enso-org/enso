<script setup lang="ts">
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import TopBar from '@/components/TopBar.vue'

import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { Rect } from '@/stores/rect'
import { modKey, usePointer, useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { onMounted, onUnmounted, reactive, ref } from 'vue'
import { shortcutRegistry } from '../util/shortcuts'
import CustomCursor from './CustomCursor.vue'
import SelectionBrush from './SelectionBrush.vue'

const EXECUTION_MODES = ['design', 'live']

const title = ref('Test Project')
const mode = ref('design')
const viewportNode = ref<HTMLElement>()
const navigator = useNavigator(viewportNode)
const graphStore = useGraphStore()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserPosition = ref(Vec2.Zero())

const nodeRects = reactive(new Map<ExprId, Rect>())
const exprRects = reactive(new Map<ExprId, Rect>())
const selectedNodes = reactive(new Set<ExprId>())

function updateNodeRect(id: ExprId, rect: Rect) {
  nodeRects.set(id, rect)
}

function updateExprRect(id: ExprId, rect: Rect) {
  exprRects.set(id, rect)
}

function keyboardBusy() {
  return document.activeElement != document.body
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

const cursorPos = ref({ x: 0, y: 0 })
const selectionStart = ref<Vec2>()
const selectionSize = ref<Vec2>()

const selection = usePointer((pos) => {
  cursorPos.value = pos.absolute
  selectionStart.value = pos.initial
  selectionSize.value = pos.relative
})

const unregisterKeyboardHandlers = ref<() => void>()

onMounted(() => {
  shortcutRegistry.registerKeyboardHandlers({
    'select-all-nodes': () => {
      for (const id of graphStore.nodes.keys()) {
        selectedNodes.add(id)
      }
    },
    'deselect-all-nodes': () => {
      selectedNodes.clear()
    },
  })
})

onUnmounted(() => {
  unregisterKeyboardHandlers.value?.()
})
</script>

<template>
  <div
    ref="viewportNode"
    class="viewport"
    :class="{ selecting: selection.dragging }"
    v-on="navigator.events"
    @pointerdown="
      selection.events.pointerdown($event), $event.button === 0 && selectedNodes.clear()
    "
    @mousemove="(cursorPos.x = $event.clientX), (cursorPos.y = $event.clientY)"
  >
    <svg :viewBox="navigator.viewBox">
      <GraphEdge
        v-for="(edge, index) in graphStore.edges"
        :key="index"
        :edge="edge"
        :node-rects="nodeRects"
        :expr-rects="exprRects"
        :expr-nodes="graphStore.exprNodes"
      />
    </svg>
    <div :style="{ transform: navigator.transform }" class="htmlLayer">
      <GraphNode
        v-for="[id, node] in graphStore.nodes"
        :key="id"
        :node="node"
        :selected="selectedNodes.has(id)"
        @update:selected="$event ? selectedNodes.add(id) : selectedNodes.delete(id)"
        @replaceSelection="selectedNodes.clear(), selectedNodes.add(id)"
        @updateRect="updateNodeRect(id, $event)"
        @delete="graphStore.deleteNode(id)"
        @updateExprRect="updateExprRect"
        @updateContent="(range, c) => updateNodeContent(id, range, c)"
        @movePosition="moveNode(id, $event)"
      />
    </div>
    <SelectionBrush
      v-if="selection.dragging && selectionStart && selectionSize"
      :start="selectionStart"
      :size="selectionSize"
    />
    <CustomCursor v-else v-bind="cursorPos" />
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
  </div>
</template>

<style scoped>
.viewport {
  position: relative;
  contain: layout;
  overflow: clip;
  cursor: none;
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
