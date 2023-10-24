<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import SelectionBrush from '@/components/SelectionBrush.vue'
import TopBar from '@/components/TopBar.vue'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { useGraphStore, type Edge } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/util/events'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import { type ExprId } from 'shared/yjsModel.ts'
import { computed, onMounted, ref, watch } from 'vue'
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

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    const name = group.name.replace(/\s/g, '-')
    let color = group.color ?? colorFromString(name)
    styles[`--group-color-${name}`] = color
  }
  return styles
})

abstract class Interaction {
  id: number
  static nextId: number = 0
  constructor() {
    this.id = Interaction.nextId
    Interaction.nextId += 1
  }

  abstract cancel(): void
  click(_e: MouseEvent): boolean {
    return false
  }
}
const currentInteraction = ref<Interaction>()
class EditingEdge extends Interaction {
  cancel() {
    const target = graphStore.unconnectedEdge?.disconnectedEdgeTarget
    graphStore.transact(() => {
      if (target != null)
        disconnectEdge(target)
      graphStore.clearUnconnected()
    })
  }
  click(_e: MouseEvent): boolean {
    return onClick(_e)
  }
}
const editingEdge = new EditingEdge()
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
function forgetInteraction(interaction: Interaction) {
  if (currentInteraction.value?.id === interaction?.id) currentInteraction.value = undefined
}

watch(
  () => graphStore.unconnectedEdge,
  (edge) => {
    if (edge != null) {
      setCurrentInteraction(editingEdge)
    } else {
      forgetInteraction(editingEdge)
    }
  },
)
watch(componentBrowserVisible, (visible) => {
  if (visible) {
    setCurrentInteraction(editingNode)
  } else {
    forgetInteraction(editingNode)
  }
})

const hoveredNode = ref<ExprId>()
const hoveredExpr = ref<ExprId>()

function onClick(_e: MouseEvent) {
  if (graphStore.unconnectedEdge == null) return false
  const source = graphStore.unconnectedEdge.source ?? hoveredNode.value
  const target = graphStore.unconnectedEdge.target ?? hoveredExpr.value
  const targetNode = target != null ? graphStore.exprNodes.get(target) : undefined
  graphStore.transact(() => {
    if (source != targetNode) {
      if (target == null && graphStore.unconnectedEdge?.disconnectedEdgeTarget != null) {
        disconnectEdge(graphStore.unconnectedEdge.disconnectedEdgeTarget)
      }
      if (source == null || target == null) {
        createNodeFromEdgeDrop({ source, target })
      } else {
        createEdge(source, target)
      }
    }
    graphStore.clearUnconnected()
  })
  return true
}

function disconnectEdge(target: ExprId) {
  graphStore.setExpressionContent(target, '_')
}
function createNodeFromEdgeDrop(edge: Edge) {
  console.log(`TODO: createNodeFromEdgeDrop(${JSON.stringify(edge)})`)
}
function createEdge(source: ExprId, target: ExprId) {
  const sourceNode = graphStore.nodes.get(source)
  if (sourceNode == null) return
  // TODO: Check alias analysis to see if the binding is shadowed.
  graphStore.setExpressionContent(target, sourceNode.binding)
  // TODO: Use alias analysis to ensure declarations are in a dependency order.
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
  >
    <svg :viewBox="navigator.viewBox">
      <GraphEdges
        :sceneMousePos="navigator.sceneMousePos"
        :hoveredNode="hoveredNode"
        :hoveredExpr="hoveredExpr"
      />
    </svg>
    <div :style="{ transform: navigator.transform }" class="htmlLayer">
      <GraphNodes @hoverNode="hoveredNode = $event" @hoverExpr="hoveredExpr = $event" />
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
