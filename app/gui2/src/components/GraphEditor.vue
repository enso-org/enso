<script lang="ts">
import { codeEditorBindings, graphBindings, nodeSelectionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import GraphEdge from '@/components/GraphEdge.vue'
import GraphNode from '@/components/GraphNode.vue'
import SelectionBrush from '@/components/SelectionBrush.vue'
import TopBar from '@/components/TopBar.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent, usePointer } from '@/util/events'
import { useNavigator } from '@/util/navigator'
import type { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import type { ContentRange, ExprId } from 'shared/yjsModel'
import { computed, onMounted, reactive, ref, shallowRef, watch } from 'vue'
</script>
<script setup lang="ts">
const EXECUTION_MODES = ['design', 'live']
const SELECTION_BRUSH_MARGIN_PX = 6

const mode = ref('design')
const viewportNode = ref<HTMLElement>()
const navigator = useNavigator(viewportNode)
const graphStore = useGraphStore()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserPosition = ref(Vec2.Zero())
const suggestionDb = useSuggestionDbStore()

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

useEvent(window, 'keydown', (event) => {
  graphBindingsHandler(event) || nodeSelectionHandler(event) || codeEditorHandler(event)
})

onMounted(() => viewportNode.value?.focus())

function updateNodeContent(id: ExprId, updates: [ContentRange, string][]) {
  graphStore.transact(() => {
    for (const [range, content] of updates) {
      graphStore.replaceNodeSubexpression(id, range, content)
    }
  })
}

function moveNode(id: ExprId, delta: Vec2) {
  const scaledDelta = delta.scale(1 / navigator.scale)
  graphStore.transact(() => {
    for (const id_ of selectedNodes.value.has(id) ? selectedNodes.value : [id]) {
      const node = graphStore.nodes.get(id_)
      if (node == null) {
        continue
      }
      graphStore.setNodePosition(id_, node.position.add(scaledDelta))
    }
  })
}

const selectionAnchor = shallowRef<Vec2>()
const initiallySelectedNodes = ref(new Set(selectedNodes.value))

const selection = usePointer((pos, _, eventType) => {
  if (selection.dragging && selectionAnchor.value == null) {
    selectionAnchor.value = navigator.sceneMousePos?.copy()
  } else if (eventType === 'stop') {
    selectionAnchor.value = undefined
  }
})

const intersectingNodes = computed<Set<ExprId>>(() => {
  if (!selection.dragging || selectionAnchor.value == null || navigator.sceneMousePos == null) {
    return new Set()
  }
  const margin = SELECTION_BRUSH_MARGIN_PX / navigator.scale

  const a = navigator.sceneMousePos
  const b = selectionAnchor.value

  const left = Math.min(a.x, b.x) - margin
  const right = Math.max(a.x, b.x) + margin
  const top = Math.min(a.y, b.y) - margin
  const bottom = Math.max(a.y, b.y) + margin
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

const graphBindingsHandler = graphBindings.handler({
  undo() {
    projectStore.module?.undoManager.undo()
  },
  redo() {
    projectStore.module?.undoManager.redo()
  },
  openComponentBrowser() {
    if (keyboardBusy()) return false
    if (navigator.sceneMousePos != null && !componentBrowserVisible.value) {
      componentBrowserPosition.value = navigator.sceneMousePos
      componentBrowserVisible.value = true
    }
  },
  newNode() {
    if (keyboardBusy()) return false
    if (navigator.sceneMousePos != null) {
      graphStore.createNode(navigator.sceneMousePos, 'hello "world"! 123 + x')
    }
  },
})

const codeEditorArea = ref<HTMLElement>()
const showCodeEditor = ref(false)
const codeEditorHandler = codeEditorBindings.handler({
  toggle() {
    if (keyboardBusyExceptIn(codeEditorArea.value)) return false
    showCodeEditor.value = !showCodeEditor.value
  },
})

const nodeSelectionHandler = nodeSelectionBindings.handler({
  deleteSelected() {
    graphStore.transact(() => {
      for (const node of selectedNodes.value) {
        graphStore.deleteNode(node)
      }
    })
  },
  selectAll() {
    if (keyboardBusy()) return
    for (const id of graphStore.nodes.keys()) {
      selectedNodes.value.add(id)
    }
  },
  deselectAll() {
    clearSelection()
    selectedNodes.value.clear()
    graphStore.stopCapturingUndo()
  },
  toggleVisualization() {
    if (keyboardBusy()) return false
    graphStore.transact(() => {
      const allHidden = set
        .toArray(selectedNodes.value)
        .every((id) => graphStore.nodes.get(id)?.vis?.visible !== true)

      for (const nodeId of selectedNodes.value) {
        graphStore.setNodeVisualizationVisible(nodeId, allHidden)
      }
    })
  },
})

const mouseHandler = nodeSelectionBindings.handler({
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
})

const scaledMousePos = computed(() => navigator.sceneMousePos?.scale(navigator.scale))
const scaledSelectionAnchor = computed(() => selectionAnchor.value?.scale(navigator.scale))

function clearSelection() {
  selectedNodes.value.clear()
  latestSelectedNode.value = undefined
  if (document.activeElement instanceof HTMLElement) {
    document.activeElement.blur()
  }
}

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    const name = group.name.replace(/\s/g, '-')
    let color = group.color ?? colorFromString(name)
    styles[`--group-color-${name}`] = color
  }
  return styles
})
</script>

<template>
  <div
    ref="viewportNode"
    class="viewport"
    v-on.="navigator.events"
    v-on..="selection.events"
    :style="groupColors"
    @pointerdown="nodeSelectionHandler"
    @pointermove="selection.dragging && mouseHandler($event)"
  >
    <svg :viewBox="navigator.viewBox">
      <GraphEdge
        v-for="(edge, index) in graphStore.edges"
        :key="index"
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
        :selected="selectedNodes.has(id)"
        :isLatestSelected="id === latestSelectedNode"
        :fullscreenVis="false"
        @update:selected="setSelected(id, $event), $event && updateLatestSelectedNode(id)"
        @replaceSelection="
          selectedNodes.clear(), selectedNodes.add(id), updateLatestSelectedNode(id)
        "
        @updateRect="updateNodeRect(id, $event)"
        @delete="graphStore.deleteNode(id)"
        @updateExprRect="updateExprRect"
        @updateContent="updateNodeContent(id, $event)"
        @setVisualizationId="graphStore.setNodeVisualizationId(id, $event)"
        @setVisualizationVisible="graphStore.setNodeVisualizationVisible(id, $event)"
        @movePosition="moveNode(id, $event)"
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
      :title="projectStore.name"
      :modes="EXECUTION_MODES"
      :breadcrumbs="['main', 'ad_analytics']"
      @breadcrumbClick="console.log(`breadcrumb #${$event + 1} clicked.`)"
      @back="console.log('breadcrumbs \'back\' button clicked.')"
      @forward="console.log('breadcrumbs \'forward\' button clicked.')"
      @execute="console.log('\'execute\' button clicked.')"
    />
    <div ref="codeEditorArea">
      <Suspense>
        <Transition>
          <CodeEditor v-if="showCodeEditor" />
        </Transition>
      </Suspense>
    </div>
    <SelectionBrush
      v-if="scaledMousePos"
      :position="scaledMousePos"
      :anchor="scaledSelectionAnchor"
      :style="{ transform: navigator.prescaledTransform }"
    />
  </div>
</template>

<style scoped>
.viewport {
  position: relative;
  contain: layout;
  overflow: clip;
  cursor: none;
  --group-color-fallback: #006b8a;
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
</style>
