<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import {
  mouseDictatedPlacement,
  nonDictatedPlacement,
  previousNodeDictatedPlacement,
  type Environment,
} from '@/components/ComponentBrowser/placement.ts'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import GraphMouse from '@/components/GraphMouse.vue'
import PlusButton from '@/components/PlusButton.vue'
import TopBar from '@/components/TopBar.vue'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/util/events'
import { Rect } from '@/util/rect.ts'
import { Vec2 } from '@/util/vec2'
import * as set from 'lib0/set'
import type { ExprId, NodeMetadata } from 'shared/yjsModel.ts'
import { computed, onMounted, ref, watch } from 'vue'

const EXECUTION_MODES = ['design', 'live']
// Difference in position between the component browser and a node for the input of the component browser to
// be placed at the same position as the node.
const COMPONENT_BROWSER_TO_NODE_OFFSET = new Vec2(20, 35)
// Assumed size of a newly created node. This is used to place the component browser.
const DEFAULT_NODE_SIZE = new Vec2(0, 24)
const gapBetweenNodes = 48.0

const viewportNode = ref<HTMLElement>()
const graphNavigator = provideGraphNavigator(viewportNode)
const graphStore = useGraphStore()
const widgetRegistry = provideWidgetRegistry(graphStore.db)
widgetRegistry.loadBuiltins()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserInputContent = ref('')
const suggestionDb = useSuggestionDbStore()
const interaction = provideInteractionHandler()

const nodeSelection = provideGraphSelection(graphNavigator, graphStore.nodeRects, {
  onSelected(id) {
    graphStore.db.moveNodeToTop(id)
  },
})

const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
  click: (e) => (e instanceof MouseEvent ? interaction.handleClick(e, graphNavigator) : false),
})

// Return the environment for the placement of a new node. The passed nodes should be the nodes that are
// used as the source of the placement. This means, for example, the selected nodes when creating from a selection
// or the node that is being edited when creating from a port double click.
function environmentForNodes(nodeIds: IterableIterator<ExprId>): Environment {
  const nodeRects = [...graphStore.nodeRects.values()]
  const selectedNodeRects = [...nodeIds]
    .map((id) => graphStore.nodeRects.get(id))
    .filter((item): item is Rect => item !== undefined)
  const screenBounds = graphNavigator.viewport
  const mousePosition = graphNavigator.sceneMousePos
  return { nodeRects, selectedNodeRects, screenBounds, mousePosition } as Environment
}

const placementEnvironment = computed(() => environmentForNodes(nodeSelection.selected.values()))

/** Return the position for a new node, assuming there are currently nodes selected. If there are no nodes
 * selected, return `undefined`. */
function placementPositionForSelection() {
  const hasNodeSelected = nodeSelection.selected.size > 0
  if (!hasNodeSelected) return
  const gapBetweenNodes = 48.0
  return previousNodeDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value, {
    horizontalGap: gapBetweenNodes,
    verticalGap: gapBetweenNodes,
  }).position
}

/** Where the component browser should be placed when it is opened. */
function targetComponentBrowserPosition() {
  const editedInfo = graphStore.editedNodeInfo
  const isEditingNode = editedInfo != null
  if (isEditingNode) {
    const targetNode = graphStore.db.nodeIdToNode.get(editedInfo.id)
    const targetPos = targetNode?.position ?? Vec2.Zero
    return targetPos.add(COMPONENT_BROWSER_TO_NODE_OFFSET)
  } else {
    return (
      placementPositionForSelection() ??
      mouseDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value).position
    )
  }
}

/** The current position of the component browser. */
const componentBrowserPosition = ref<Vec2>(Vec2.Zero)

function sourcePortForSelection() {
  if (graphStore.editedNodeInfo != null) return undefined
  const firstSelectedNode = set.first(nodeSelection.selected)
  return graphStore.db.getNodeFirstOutputPort(firstSelectedNode)
}

const componentBrowserSourcePort = ref<ExprId | undefined>(sourcePortForSelection())

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
  startProfiling() {
    projectStore.lsRpcConnection.then((ls) => ls.profilingStart(true))
  },
  stopProfiling() {
    projectStore.lsRpcConnection.then((ls) => ls.profilingStop())
  },
  openComponentBrowser() {
    if (keyboardBusy()) return false
    if (graphNavigator.sceneMousePos != null && !componentBrowserVisible.value) {
      interaction.setCurrent(creatingNode)
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
  zoomToSelected() {
    if (!viewportNode.value) return
    let left = Infinity
    let top = Infinity
    let right = -Infinity
    let bottom = -Infinity
    const nodesToCenter =
      nodeSelection.selected.size === 0 ? graphStore.nodeRects.keys() : nodeSelection.selected
    for (const id of nodesToCenter) {
      const rect = graphStore.nodeRects.get(id)
      if (!rect) continue
      left = Math.min(left, rect.left)
      right = Math.max(right, rect.right)
      top = Math.min(top, rect.top)
      bottom = Math.max(bottom, rect.bottom)
    }
    graphNavigator.panAndZoomTo(
      Rect.FromBounds(left, top, right, bottom),
      0.1,
      Math.max(1, graphNavigator.scale),
    )
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
        .every((id) => !(graphStore.db.nodeIdToNode.get(id)?.vis?.visible !== true))

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

/** Track play button presses. */
function onPlayButtonPress() {
  projectStore.lsRpcConnection.then(async () => {
    const modeValue = projectStore.executionMode
    if (modeValue == undefined) {
      return
    }
    projectStore.executionContext.recompute('all', modeValue === 'live' ? 'Live' : 'Design')
  })
}

// Watch for changes in the execution mode.
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
  init: () => {
    componentBrowserPosition.value = targetComponentBrowserPosition()
  },
  cancel: () => (componentBrowserVisible.value = false),
}
const nodeIsBeingEdited = computed(() => graphStore.editedNodeInfo != null)
interaction.setWhen(nodeIsBeingEdited, editingNode)

const creatingNode: Interaction = {
  init: () => {
    componentBrowserInputContent.value = ''
    componentBrowserSourcePort.value = sourcePortForSelection()
    componentBrowserPosition.value = targetComponentBrowserPosition()
    componentBrowserVisible.value = true
  },
}

const creatingNodeFromButton: Interaction = {
  init: () => {
    componentBrowserInputContent.value = ''
    let targetPos = placementPositionForSelection()
    if (targetPos == undefined) {
      targetPos = nonDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value).position
    }
    componentBrowserPosition.value = targetPos
    componentBrowserVisible.value = true
  },
}

const creatingNodeFromPortDoubleClick: Interaction = {
  init: () => {
    componentBrowserInputContent.value = ''
    componentBrowserVisible.value = true
  },
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

function resetComponentBrowserState() {
  componentBrowserVisible.value = false
  graphStore.editedNodeInfo = undefined
  interaction.setCurrent(undefined)
}

function onComponentBrowserCommit(content: string, requiredImports: RequiredImport[]) {
  if (content != null) {
    if (graphStore.editedNodeInfo) {
      // We finish editing a node.
      graphStore.setNodeContent(graphStore.editedNodeInfo.id, content)
    } else {
      // We finish creating a new node.
      const nodePosition = componentBrowserPosition.value
      const metadata = undefined
      graphStore.createNode(
        nodePosition.sub(COMPONENT_BROWSER_TO_NODE_OFFSET),
        content,
        metadata,
        requiredImports,
      )
    }
  }
  resetComponentBrowserState()
}

const onComponentBrowserCancel = resetComponentBrowserState

function getNodeContent(id: ExprId): string {
  return graphStore.db.nodeIdToNode.get(id)?.rootSpan.repr() ?? ''
}

// Watch the `editedNode` in the graph store
watch(
  () => graphStore.editedNodeInfo,
  (editedInfo) => {
    if (editedInfo) {
      componentBrowserPosition.value = targetComponentBrowserPosition()
      componentBrowserInputContent.value = getNodeContent(editedInfo.id)
      componentBrowserVisible.value = true
    } else {
      componentBrowserVisible.value = false
    }
  },
)

const breadcrumbs = computed(() =>
  projectStore.executionContext.desiredStack.map((frame) => {
    switch (frame.type) {
      case 'ExplicitCall':
        return frame.methodPointer.name
      case 'LocalCall':
        return frame.expressionId
    }
  }),
)

// === Clipboard ===

const ENSO_MIME_TYPE = 'web application/enso'

/** The data that is copied to the clipboard. */
interface ClipboardData {
  nodes: CopiedNode[]
}

/** Node data that is copied to the clipboard. Used for serializing and deserializing the node information. */
interface CopiedNode {
  expression: string
  metadata: NodeMetadata | undefined
}

/** Copy the content of the selected node to the clipboard. */
function copyNodeContent() {
  const id = nodeSelection.selected.values().next().value
  const node = graphStore.db.nodeIdToNode.get(id)
  if (!node) return
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
  if (!clipboardData) {
    console.warn('No valid data in clipboard.')
    return
  }
  const copiedNode = clipboardData.nodes[0]
  if (!copiedNode) {
    console.warn('No valid node in clipboard.')
    return
  }
  if (copiedNode.expression == null) {
    console.warn('No valid expression in clipboard.')
  }
  graphStore.createNode(
    graphNavigator.sceneMousePos ?? Vec2.Zero,
    copiedNode.expression,
    copiedNode.metadata,
  )
}

function handleNodeOutputPortDoubleClick(id: ExprId) {
  componentBrowserSourcePort.value = id
  const placementEnvironment = environmentForNodes([id].values())
  componentBrowserPosition.value = previousNodeDictatedPlacement(
    DEFAULT_NODE_SIZE,
    placementEnvironment,
    {
      horizontalGap: gapBetweenNodes,
      verticalGap: gapBetweenNodes,
    },
  ).position
  interaction.setCurrent(creatingNodeFromPortDoubleClick)
}
</script>

<template>
  <div
    ref="viewportNode"
    class="GraphEditor viewport"
    :class="{ draggingEdge: graphStore.unconnectedEdge != null }"
    :style="groupColors"
    v-on.="graphNavigator.events"
    v-on..="nodeSelection.events"
    @click="graphBindingsHandler"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <svg :viewBox="graphNavigator.viewBox">
      <GraphEdges />
    </svg>
    <div :style="{ transform: graphNavigator.transform }" class="htmlLayer">
      <GraphNodes @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick" />
    </div>
    <ComponentBrowser
      v-if="componentBrowserVisible"
      ref="componentBrowser"
      :navigator="graphNavigator"
      :position="componentBrowserPosition"
      :initialContent="componentBrowserInputContent"
      :initialCaretPosition="graphStore.editedNodeInfo?.range ?? [0, 0]"
      :sourcePort="componentBrowserSourcePort"
      @accepted="onComponentBrowserCommit"
      @closed="onComponentBrowserCancel"
      @canceled="onComponentBrowserCancel"
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
    <PlusButton @pointerdown="interaction.setCurrent(creatingNodeFromButton)" />
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
