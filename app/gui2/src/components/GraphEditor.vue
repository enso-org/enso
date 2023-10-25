<script setup lang="ts">
import { codeEditorBindings, graphBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import SelectionBrush from '@/components/SelectionBrush.vue'
import TopBar from '@/components/TopBar.vue'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/util/events'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import { computed, onMounted, ref } from 'vue'
import GraphEdges from './GraphEditor/GraphEdges.vue'
import GraphNodes from './GraphEditor/GraphNodes.vue'

const EXECUTION_MODES = ['design', 'live']

const mode = ref('design')
const viewportNode = ref<HTMLElement>()
const navigator = provideGraphNavigator(viewportNode)
const graphStore = useGraphStore()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserPosition = ref(Vec2.Zero())
const suggestionDb = useSuggestionDbStore()

const nodeSelection = provideGraphSelection(navigator, graphStore.nodeRects, {
  onSelected(id) {
    const node = graphStore.nodes.get(id)
    if (node) {
      // When a node is selected, we want to reorder it to be visually at the top. This is done by
      // reinserting it into the nodes map, which is later iterated over in the template.
      graphStore.nodes.delete(id)
      graphStore.nodes.set(id, node)
    }
  },
})

useEvent(window, 'keydown', (event) => {
  graphBindingsHandler(event) || codeEditorHandler(event)
})

onMounted(() => viewportNode.value?.focus())

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
  deleteSelected() {
    graphStore.transact(() => {
      for (const node of nodeSelection.selected) {
        graphStore.deleteNode(node)
      }
    })
  },
  selectAll() {
    if (keyboardBusy()) return
    nodeSelection.selectAll()
  },
  deselectAll() {
    nodeSelection.deselectAll()
    if (document.activeElement instanceof HTMLElement) {
      document.activeElement.blur()
    }
    graphStore.stopCapturingUndo()
  },
  toggleVisualization() {
    if (keyboardBusy()) return false
    graphStore.transact(() => {
      const allVisible = set
        .toArray(nodeSelection.selected)
        .every((id) => !(graphStore.nodes.get(id)?.vis?.visible !== true))

      for (const nodeId of nodeSelection.selected) {
        graphStore.setNodeVisualizationVisible(nodeId, !allVisible)
      }
    })
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

const scaledMousePos = computed(() => navigator.sceneMousePos?.scale(navigator.scale))
const scaledSelectionAnchor = computed(() => nodeSelection.anchor?.scale(navigator.scale))

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
  <!-- eslint-disable vue/attributes-order -->
  <div
    ref="viewportNode"
    class="viewport"
    :style="groupColors"
    @click="graphBindingsHandler"
    v-on.="navigator.events"
    v-on..="nodeSelection.events"
  >
    <svg :viewBox="navigator.viewBox">
      <GraphEdges />
    </svg>
    <div :style="{ transform: navigator.transform }" class="htmlLayer">
      <GraphNodes />
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
