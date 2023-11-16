<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import {
  mouseDictatedPlacement,
  type Environment,
} from '@/components/ComponentBrowser/placement.ts'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import TopBar from '@/components/TopBar.vue'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/util/events'
import type { Rect } from '@/util/rect.ts'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import type { ExprId, NodeMetadata } from 'shared/yjsModel.ts'
import { computed, onMounted, ref, watch } from 'vue'
import GraphEdges from './GraphEditor/GraphEdges.vue'
import GraphNodes from './GraphEditor/GraphNodes.vue'
import GraphMouse from './GraphMouse.vue'

const EXECUTION_MODES = ['design', 'live']

const viewportNode = ref<HTMLElement>()
const graphNavigator = provideGraphNavigator(viewportNode)
const graphStore = useGraphStore()
const widgetRegistry = provideWidgetRegistry(graphStore.db)
widgetRegistry.loadBuiltins()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserInputContent = ref('')
const componentBrowserPosition = ref(Vec2.Zero)
const suggestionDb = useSuggestionDbStore()
const interaction = provideInteractionHandler()

const nodeSelection = provideGraphSelection(graphNavigator, graphStore.nodeRects, {
  onSelected(id) {
    graphStore.db.moveNodeToTop(id)
  },
})

const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
  click: (e) => (e instanceof MouseEvent ? interaction.handleClick(e) : false),
})

const graphEditorSourceNode = computed(() => {
  if (graphStore.editedNodeInfo != null) return undefined
  return nodeSelection.selected.values().next().value
})

useEvent(window, 'keydown', (event) => {
  interactionBindingsHandler(event) || graphBindingsHandler(event) || codeEditorHandler(event)
})
useEvent(window, 'pointerdown', interactionBindingsHandler, { capture: true })

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
    if (graphNavigator.sceneMousePos != null && !componentBrowserVisible.value) {
      componentBrowserPosition.value = graphNavigator.sceneMousePos
      interaction.setCurrent(new CreatingNode())
    }
  },
  newNode() {
    if (keyboardBusy()) return false
    if (graphNavigator.sceneMousePos != null) {
      graphStore.createNode(graphNavigator.sceneMousePos, 'hello "world"! 123 + x')
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
    console.log('deselectAll')
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
        .every((id) => !(graphStore.db.getNode(id)?.vis?.visible !== true))

      for (const nodeId of nodeSelection.selected) {
        graphStore.setNodeVisualizationVisible(nodeId, !allVisible)
      }
    })
  },
  copyNode() {
    if (keyboardBusy()) return false
    copyNodeContent()
  },
  pasteNode() {
    if (keyboardBusy()) return false
    readNodeFromClipboard()
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
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name.replace(/\w/g, '-'))
  }
  return styles
})

const editingNode: Interaction = {
  cancel: () => (componentBrowserVisible.value = false),
}
interaction.setWhen(componentBrowserVisible, editingNode)

const placementEnvironment = computed(() => {
  const mousePosition = graphNavigator.sceneMousePos ?? Vec2.Zero
  const nodeRects = [...graphStore.nodeRects.values()]
  const selectedNodesIter = nodeSelection.selected.values()
  const selectedNodeRects: Iterable<Rect> = [...selectedNodesIter]
    .map((id) => graphStore.nodeRects.get(id))
    .filter((item): item is Rect => item !== undefined)
  const screenBounds = graphNavigator.viewport
  const environment: Environment = { mousePosition, nodeRects, selectedNodeRects, screenBounds }
  return environment
})

/// Interaction to create a new node. This will create a temporary node and open the component browser.
/// If the interaction is cancelled, the temporary node will be deleted, otherwise it will be kept.
class CreatingNode implements Interaction {
  nodeId: ExprId
  // Start a node creation interaction. This will create a new node and open the component browser.
  // For more information about the flow of the interaction, see `CreatingNode`.
  constructor() {
    // We create a temporary node to show the component browser on. This node will be deleted if
    // the interaction is cancelled. It can later on be used to have a preview of the node as it is
    // being created.
    const nodeHeight = 32
    const targetPosition = mouseDictatedPlacement(
      Vec2.FromArr([0, nodeHeight]),
      placementEnvironment.value,
    )
    const nodeId = graphStore.createNode(targetPosition.position, '')
    if (nodeId == null) {
      throw new Error('CreatingNode: Failed to create node.')
    }
    this.nodeId = nodeId
    // From here on we just edit the temporary node.
    graphStore.editedNodeInfo = { id: nodeId, range: [0, 0] }
  }
  cancel() {
    // Aborting node creation means we no longer need the temporary node.
    graphStore.deleteNode(this.nodeId)
  }
}

async function handleFileDrop(event: DragEvent) {
  // A vertical gap between created nodes when multiple files were dropped together.
  const MULTIPLE_FILES_GAP = 50

  try {
    if (event.dataTransfer && event.dataTransfer.items) {
      ;[...event.dataTransfer.items].forEach(async (item, index) => {
        if (item.kind === 'file') {
          const file = item.getAsFile()
          if (file) {
            const clientPos = new Vec2(event.clientX, event.clientY)
            const offset = new Vec2(0, index * -MULTIPLE_FILES_GAP)
            const pos = graphNavigator.clientToScenePos(clientPos).add(offset)
            const uploader = await Uploader.Create(
              projectStore.lsRpcConnection,
              projectStore.dataConnection,
              projectStore.contentRoots,
              projectStore.awareness,
              file,
              pos,
              projectStore.executionContext.getStackTop(),
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

function onComponentBrowserCommit(content: string) {
  if (content != null && graphStore.editedNodeInfo != null) {
    graphStore.setNodeContent(graphStore.editedNodeInfo.id, content)
  }
  componentBrowserVisible.value = false
  graphStore.editedNodeInfo = undefined
}

/**
 *
 */
function getNodeContent(id: ExprId): string {
  const node = graphStore.db.nodes.get(id)
  if (node == null) return ''
  return node.rootSpan.repr()
}

// Watch the editedNode in the graph store
watch(
  () => graphStore.editedNodeInfo,
  (editedInfo) => {
    if (editedInfo != null) {
      const targetNode = graphStore.db.nodes.get(editedInfo.id)
      const targetPos = targetNode?.position ?? Vec2.Zero
      const offset = new Vec2(20, 35)
      componentBrowserPosition.value = targetPos.add(offset)
      componentBrowserInputContent.value = getNodeContent(editedInfo.id)
      componentBrowserVisible.value = true
    } else {
      componentBrowserVisible.value = false
    }
  },
)

const breadcrumbs = computed(() => {
  return projectStore.executionContext.desiredStack.map((frame) => {
    switch (frame.type) {
      case 'ExplicitCall':
        return frame.methodPointer.name
      case 'LocalCall':
        return frame.expressionId
    }
  })
})

/// === Clipboard ===

const ENSO_MIME_TYPE = 'web application/enso'

/// The data that is copied to the clipboard.
interface ClipboardData {
  nodes: CopiedNode[]
}

/// Node data that is copied to the clipboard. Used for serializing and deserializing the node information.
interface CopiedNode {
  expression: string
  metadata: NodeMetadata | undefined
}

/// Copy the content of the selected node to the clipboard.
function copyNodeContent() {
  const id = nodeSelection.selected.values().next().value
  const node = graphStore.db.nodes.get(id)
  if (node == null) return
  const content = node.rootSpan.repr()
  const metadata = projectStore.module?.getNodeMetadata(id) ?? undefined
  const copiedNode: CopiedNode = { expression: content, metadata }
  const clipboardData: ClipboardData = { nodes: [copiedNode] }
  const jsonItem = new Blob([JSON.stringify(clipboardData)], { type: ENSO_MIME_TYPE })
  const textItem = new Blob([content], { type: 'text/plain' })
  const clipboardItem = new ClipboardItem({ [jsonItem.type]: jsonItem, [textItem.type]: textItem })
  navigator.clipboard.write([clipboardItem])
}

async function retrieveDataFromClipboard(): Promise<ClipboardData | undefined> {
  const clipboardItems = await navigator.clipboard.read()
  let fallback = undefined
  for (const clipboardItem of clipboardItems) {
    for (const type of clipboardItem.types) {
      if (type === ENSO_MIME_TYPE) {
        const blob = await clipboardItem.getType(type)
        return JSON.parse(await blob.text())
      }
      if (type === 'text/plain') {
        const blob = await clipboardItem.getType(type)
        const fallbackExpression = await blob.text()
        const fallbackNode = { expression: fallbackExpression, metadata: undefined } as CopiedNode
        fallback = { nodes: [fallbackNode] } as ClipboardData
      }
    }
  }
  return fallback
}

/// Read the clipboard and if it contains valid data, create a node from the content.
async function readNodeFromClipboard() {
  let clipboardData = await retrieveDataFromClipboard()
  if (clipboardData == undefined) {
    console.warn('No valid data in clipboard.')
    return
  }
  const copiedNode = clipboardData.nodes[0]
  if (copiedNode == undefined) {
    console.warn('No valid node in clipboard.')
    return
  }
  if (copiedNode.expression != null) {
    graphStore.createNode(
      graphNavigator.sceneMousePos ?? Vec2.Zero,
      copiedNode.expression,
      copiedNode.metadata,
    )
  } else {
    console.warn('No valid expression in clipboard.')
  }
}
</script>

<template>
  <!-- eslint-disable vue/attributes-order -->
  <div
    ref="viewportNode"
    class="GraphEditor"
    :class="{ draggingEdge: graphStore.unconnectedEdge != null }"
    :style="groupColors"
    @click="graphBindingsHandler"
    v-on.="graphNavigator.events"
    v-on..="nodeSelection.events"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <svg :viewBox="graphNavigator.viewBox">
      <GraphEdges />
    </svg>
    <div :style="{ transform: graphNavigator.transform }" class="htmlLayer">
      <GraphNodes />
    </div>
    <ComponentBrowser
      v-if="componentBrowserVisible"
      ref="componentBrowser"
      :navigator="graphNavigator"
      :position="componentBrowserPosition"
      @finished="onComponentBrowserCommit"
      :initialContent="componentBrowserInputContent"
      :initialCaretPosition="graphStore.editedNodeInfo?.range ?? [0, 0]"
      :sourceNode="graphEditorSourceNode"
    />
    <TopBar
      v-model:mode="projectStore.executionMode"
      :title="projectStore.name"
      :modes="EXECUTION_MODES"
      :breadcrumbs="breadcrumbs"
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
    <GraphMouse />
  </div>
</template>

<style scoped>
.GraphEditor {
  position: relative;
  contain: layout;
  overflow: clip;
  cursor: none;
  --group-color-fallback: #006b8a;
  --node-color-no-type: #596b81;
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
