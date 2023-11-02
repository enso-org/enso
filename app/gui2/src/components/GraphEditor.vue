<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import SelectionBrush from '@/components/SelectionBrush.vue'
import TopBar from '@/components/TopBar.vue'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/util/events'
import { Interaction } from '@/util/interaction'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import { computed, onMounted, ref, watch } from 'vue'
import GraphEdges from './GraphEditor/GraphEdges.vue'
import GraphNodes from './GraphEditor/GraphNodes.vue'

const EXECUTION_MODES = ['design', 'live']

const viewportNode = ref<HTMLElement>()
const navigator = provideGraphNavigator(viewportNode)
const graphStore = useGraphStore()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserPosition = ref(Vec2.Zero)
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
  interactionBindingsHandler(event) || graphBindingsHandler(event) || codeEditorHandler(event)
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

const interactionBindingsHandler = interactionBindings.handler({
  cancel() {
    cancelCurrentInteraction()
  },
  click(e) {
    if (e instanceof MouseEvent) return currentInteraction.value?.click(e) ?? false
    return false
  },
})
useEvent(window, 'pointerdown', interactionBindingsHandler, { capture: true })

const scaledMousePos = computed(() => navigator.sceneMousePos?.scale(navigator.scale))
const scaledSelectionAnchor = computed(() => nodeSelection.anchor?.scale(navigator.scale))

/// Track play button presses.
function onPlayButtonPress() {
  projectStore.lsRpcConnection.then(async () => {
    const modeValue = projectStore.executionMode
    if (modeValue == undefined) {
      return
    }
    projectStore.executionContext.recompute('all', modeValue === 'live' ? 'Live' : 'Design')
  })
}

/// Watch for changes in the execution mode.
watch(
  () => projectStore.executionMode,
  (modeValue) => {
    projectStore.executionContext.setExecutionEnvironment(modeValue === 'live' ? 'Live' : 'Design')
  },
)

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    const name = group.name.replace(/\s/g, '-')
    let color = group.color ?? colorFromString(name)
    styles[`--group-color-${name}`] = color
  }
  return styles
})

const currentInteraction = ref<Interaction>()
class EditingNode extends Interaction {
  cancel() {
    componentBrowserVisible.value = false
  }
}
const editingNode = new EditingNode()

function setCurrentInteraction(interaction: Interaction | undefined) {
  if (currentInteraction.value?.id === interaction?.id) return
  currentInteraction.value?.cancel()
  currentInteraction.value = interaction
}

function cancelCurrentInteraction() {
  setCurrentInteraction(undefined)
}

/** Unset the current interaction, if it is the specified instance. */
function interactionEnded(interaction: Interaction) {
  if (currentInteraction.value?.id === interaction?.id) currentInteraction.value = undefined
}

watch(componentBrowserVisible, (visible) => {
  if (visible) {
    setCurrentInteraction(editingNode)
  } else {
    interactionEnded(editingNode)
  }
})

async function handleFileDrop(event: DragEvent) {
  try {
    if (event.dataTransfer && event.dataTransfer.items) {
      ;[...event.dataTransfer.items].forEach(async (item) => {
        if (item.kind === 'file') {
          const file = item.getAsFile()
          if (file) {
            const clientPos = new Vec2(event.clientX, event.clientY)
            const pos = navigator.clientToScenePos(clientPos)
            const uploader = await Uploader.create(
              projectStore.lsRpcConnection,
              projectStore.dataConnection,
              projectStore.contentRoots,
              file,
            )
            const name = await uploader.upload()
            graphStore.createNode(pos, uploadedExpression(name))
          }
        }
      })
    }
  } catch (err) {
    console.error(`Uploading file failed. ${err}`)
  }
}
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
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <svg :viewBox="navigator.viewBox">
      <GraphEdges @startInteraction="setCurrentInteraction" @endInteraction="interactionEnded" />
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
      v-model:mode="projectStore.executionMode"
      :title="projectStore.name"
      :modes="EXECUTION_MODES"
      :breadcrumbs="['main', 'ad_analytics']"
      @breadcrumbClick="console.log(`breadcrumb #${$event + 1} clicked.`)"
      @back="console.log('breadcrumbs \'back\' button clicked.')"
      @forward="console.log('breadcrumbs \'forward\' button clicked.')"
      @execute="onPlayButtonPress()"
    />
    <Transition>
      <Suspense ref="codeEditorArea">
        <CodeEditor v-if="showCodeEditor" />
      </Suspense>
    </Transition>
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
