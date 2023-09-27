<script lang="ts">
const graphBindings = defineKeybinds('graph-editor', {
  undo: ['Mod+Z'],
  redo: ['Mod+Y', 'Mod+Shift+Z'],
  dragScene: ['PointerAux', 'Mod+PointerMain'],
  openComponentBrowser: ['Enter'],
  newNode: ['N'],
})
</script>

<script setup lang="ts">
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import TopBar from '@/components/TopBar.vue'

import SelectionBrush from '@/components/SelectionBrush.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { Rect } from '@/stores/rect'
import { keyboardBusy, usePointer, useWindowEvent } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import { defineKeybinds } from '@/util/shortcuts'
import { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { computed, onMounted, reactive, ref, watch } from 'vue'
import { nodeBindings } from '../bindings/nodeSelection'

const EXECUTION_MODES = ['design', 'live']
const SELECTION_BRUSH_MARGIN_PX = 6

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
const latestSelectedNode = ref<ExprId>()

function updateNodeRect(id: ExprId, rect: Rect) {
  nodeRects.set(id, rect)
}

function updateExprRect(id: ExprId, rect: Rect) {
  exprRects.set(id, rect)
}

useWindowEvent('keydown', (event) => {
  if (keyboardBusy()) {
    return
  }
  graphBindingsHandler(event) || nodeSelectionHandler(event)
})

onMounted(() => viewportNode.value?.focus())

function updateNodeContent(id: ExprId, range: ContentRange, content: string) {
  graphStore.replaceNodeSubexpression(id, range, content)
}

function moveNode(id: ExprId, delta: Vec2) {
  const scaledDelta = delta.scale(1 / navigator.scale)
  for (const id_ of selectedNodes.value.has(id) ? selectedNodes.value : [id]) {
    const node = graphStore.nodes.get(id_)
    if (node == null) {
      continue
    }
    graphStore.setNodePosition(id_, node.position.add(scaledDelta))
  }
}

const selectionSize = ref<Vec2>()
const initiallySelectedNodes = ref(new Set(selectedNodes.value))

const selection = usePointer((pos, event) => {
  if (selection.dragging) {
    mouseHandler(event)
    selectionSize.value = pos.relative
  }
})

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
    }
  },
)

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

function updateLatestSelectedNode(id: ExprId) {
  latestSelectedNode.value = id
  const node = graphStore.nodes.get(id)!
  // The node MUST be deleted first, in order for it to be moved to the end of the map's iteration
  // order.
  graphStore.nodes.delete(id)
  graphStore.nodes.set(id, node)
}

const graphBindingsHandler = graphBindings.keyboardHandler({
  undo() {
    projectStore.undoManager.undo()
  },
  redo() {
    projectStore.undoManager.redo()
  },
  openComponentBrowser() {
    if (keyboardBusy()) {
      return false
    }
    if (navigator.sceneMousePos != null && !componentBrowserVisible.value) {
      componentBrowserPosition.value = navigator.sceneMousePos
      componentBrowserVisible.value = true
    }
  },
  newNode() {
    if (navigator.sceneMousePos != null) {
      graphStore.createNode(navigator.sceneMousePos, 'hello "world"! 123 + x')
    }
  },
})

const nodeSelectionHandler = nodeBindings.keyboardHandler({
  deleteSelected() {
    for (const node of selectedNodes.value) {
      graphStore.deleteNode(node)
    }
  },
  selectAll() {
    for (const id of graphStore.nodes.keys()) {
      selectedNodes.value.add(id)
    }
  },
  deselectAll() {
    selectedNodes.value.clear()
  },
})

const mouseHandler = nodeBindings.mouseHandler(
  {
    replace() {
      selectedNodes.value = new Set(intersectingNodes.value)
    },
    add() {
      selectedNodes.value = new Set([...initiallySelectedNodes.value, ...intersectingNodes.value])
    },
    remove() {
      const newSelectedNodes = new Set(initiallySelectedNodes.value)
      for (const id of intersectingNodes.value) {
        newSelectedNodes.delete(id)
      }
      selectedNodes.value = newSelectedNodes
    },
    toggle() {
      const initiallySelectedNodes_ = initiallySelectedNodes.value
      const newSelectedNodes = new Set(initiallySelectedNodes_)
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
      selectedNodes.value = newSelectedNodes
    },
    invert() {
      const initiallySelectedNodes_ = initiallySelectedNodes.value
      const newSelectedNodes = new Set(initiallySelectedNodes_)
      for (const id of intersectingNodes.value) {
        if (initiallySelectedNodes_.has(id)) {
          newSelectedNodes.delete(id)
        } else {
          newSelectedNodes.add(id)
        }
      }
      selectedNodes.value = newSelectedNodes
    },
  },
  false,
  false,
)
</script>

<template>
  <div
    ref="viewportNode"
    class="viewport"
    :class="{ selecting: selection.dragging }"
    v-on="navigator.events"
    @pointerdown="selection.events.pointerdown($event), (latestSelectedNode = undefined)"
    @pointermove="navigator.events.pointermove($event), selection.dragging && mouseHandler($event)"
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
        :is-latest-selected="id === latestSelectedNode"
        @update:selected="setSelected(id, $event), $event && updateLatestSelectedNode(id)"
        @replaceSelection="
          selectedNodes.clear(), selectedNodes.add(id), updateLatestSelectedNode(id)
        "
        @updateRect="updateNodeRect(id, $event)"
        @delete="graphStore.deleteNode(id)"
        @updateExprRect="updateExprRect"
        @updateContent="(range, c) => updateNodeContent(id, range, c)"
        @movePosition="moveNode(id, $event)"
      />
      <SelectionBrush
        v-if="navigator.sceneMousePos"
        :position="navigator.sceneMousePos"
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
    <CodeEditor ref="codeEditor" />
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
