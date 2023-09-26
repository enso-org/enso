<script setup lang="ts">
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import TopBar from '@/components/TopBar.vue'

import SelectionBrush from '@/components/SelectionBrush.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { Rect } from '@/stores/rect'
import { usePointer, useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { shortcutRegistry, type GUIMouseAction } from '@/util/shortcuts'
import { Vec2 } from '@/util/vec2'
import type { MouseAction } from 'enso-authentication/src/dashboard/shortcuts'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { computed, onMounted, onUnmounted, reactive, ref, watch, watchEffect } from 'vue'

const EXECUTION_MODES = ['design', 'live']
const SELECTION_BRUSH_MARGIN_PX = 6
const FALLBACK_CURSOR_POSITION = new Vec2(0, 0)

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
const selectedNodes = ref(new Set<ExprId>())

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

const selectionSize = ref<Vec2>()
const initiallySelectedNodes = ref(new Set(selectedNodes.value))
const mouseAction = ref<MouseAction>()

const selection = usePointer((pos) => (selectionSize.value = pos.relative))

const intersectingNodes = computed<Set<ExprId>>(() => {
  if (!selection.dragging || selectionSize.value == null) {
    return new Set()
  }
  const scaledWidth = selectionSize.value.x / navigator.scale
  const left =
    (navigator.sceneMousePos?.x ?? 0) - Math.max(scaledWidth, 0) - SELECTION_BRUSH_MARGIN_PX
  const right = left + Math.abs(scaledWidth) + 2 * SELECTION_BRUSH_MARGIN_PX
  const scaledHeight = selectionSize.value.y / navigator.scale
  const top =
    (navigator.sceneMousePos?.y ?? 0) - Math.max(scaledHeight, 0) - SELECTION_BRUSH_MARGIN_PX
  const bottom = top + Math.abs(scaledHeight) + 2 * SELECTION_BRUSH_MARGIN_PX
  const intersectingNodes = new Set<ExprId>()
  for (const [id, rect] of nodeRects) {
    const rectLeft = rect.pos.x
    const rectRight = rectLeft + rect.size.x
    const rectTop = rect.pos.y
    const rectBottom = rectTop + rect.size.y
    if (left <= rectRight && right >= rectLeft && top <= rectBottom && bottom >= rectTop) {
      intersectingNodes.add(id)
    }
  }
  return intersectingNodes
})

watch(
  () => selection.dragging,
  (dragging) => {
    if (dragging) {
      initiallySelectedNodes.value = new Set(selectedNodes.value)
    } else {
      initiallySelectedNodes.value = new Set()
      selectionSize.value = undefined
      mouseAction.value = undefined
    }
  },
)

const nodeSelectionMouseActions: GUIMouseAction[] = [
  'replace-nodes-selection',
  'add-to-nodes-selection',
  'remove-from-nodes-selection',
  'toggle-nodes-selection',
  'invert-nodes-selection',
]

function onPointerMove(event: MouseEvent) {
  if (!selection.dragging) {
    return
  }
  if (mouseAction.value == null || !shortcutRegistry.matchesMouseAction(mouseAction.value, event)) {
    mouseAction.value = nodeSelectionMouseActions.find((action) =>
      shortcutRegistry.matchesMouseAction(action, event),
    )
  }
}

watchEffect(() => {
  if (!selection.dragging || mouseAction.value == null) {
    return
  }
  let newSelectedNodes: Set<ExprId>
  switch (mouseAction.value) {
    case 'replace-nodes-selection': {
      newSelectedNodes = new Set(intersectingNodes.value)
      break
    }
    case 'add-to-nodes-selection': {
      newSelectedNodes = new Set([...initiallySelectedNodes.value, ...intersectingNodes.value])
      break
    }
    case 'remove-from-nodes-selection': {
      newSelectedNodes = new Set(initiallySelectedNodes.value)
      for (const id of intersectingNodes.value) {
        newSelectedNodes.delete(id)
      }
      break
    }
    case 'toggle-nodes-selection': {
      const initiallySelectedNodes_ = initiallySelectedNodes.value
      newSelectedNodes = new Set(initiallySelectedNodes_)
      let count = 0
      for (const id of intersectingNodes.value) {
        if (initiallySelectedNodes_.has(id)) {
          count += 1
        }
      }
      if (count * 2 <= intersectingNodes.value.size) {
        for (const id of intersectingNodes.value) {
          newSelectedNodes.add(id)
        }
      } else {
        for (const id of intersectingNodes.value) {
          newSelectedNodes.delete(id)
        }
      }
      break
    }
    case 'invert-nodes-selection': {
      const initiallySelectedNodes_ = initiallySelectedNodes.value
      newSelectedNodes = new Set(initiallySelectedNodes_)
      for (const id of intersectingNodes.value) {
        if (initiallySelectedNodes_.has(id)) {
          newSelectedNodes.delete(id)
        } else {
          newSelectedNodes.add(id)
        }
      }
      break
    }
    default: {
      return
    }
  }
  selectedNodes.value = newSelectedNodes
})

function setSelected(id: ExprId, selected: boolean) {
  if (selection.dragging) {
    if (selected) {
      initiallySelectedNodes.value.add(id)
    } else {
      initiallySelectedNodes.value.delete(id)
    }
  } else {
    if (selected) {
      selectedNodes.value.add(id)
    } else {
      selectedNodes.value.delete(id)
    }
  }
}

const unregisterKeyboardHandlers = ref<() => void>()

onMounted(() => {
  shortcutRegistry.registerKeyboardHandlers({
    undo: () => {
      projectStore.undoManager.undo()
    },
    redo: () => {
      projectStore.undoManager.redo()
    },
    'open-component-browser': () => {
      if (keyboardBusy()) {
        return false
      }
      if (navigator.sceneMousePos != null && !componentBrowserVisible.value) {
        componentBrowserPosition.value = navigator.sceneMousePos
        componentBrowserVisible.value = true
      }
    },
    'new-node': () => {
      if (navigator.sceneMousePos != null) {
        graphStore.createNode(navigator.sceneMousePos, 'hello "world"! 123 + x')
      }
    },
    'select-all-nodes': () => {
      for (const id of graphStore.nodes.keys()) {
        selectedNodes.value.add(id)
      }
    },
    'deselect-all-nodes': () => {
      selectedNodes.value.clear()
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
    @pointerdown="selection.events.pointerdown($event), onPointerMove($event)"
    @pointermove="navigator.events.pointermove($event), onPointerMove($event)"
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
        @update:selected="setSelected(id, $event)"
        @replaceSelection="selectedNodes.clear(), selectedNodes.add(id)"
        @updateRect="updateNodeRect(id, $event)"
        @delete="graphStore.deleteNode(id)"
        @updateExprRect="updateExprRect"
        @updateContent="(range, c) => updateNodeContent(id, range, c)"
        @movePosition="moveNode(id, $event)"
      />
      <SelectionBrush
        :position="navigator.sceneMousePos ?? FALLBACK_CURSOR_POSITION"
        :size="selectionSize"
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
