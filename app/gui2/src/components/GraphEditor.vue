<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import {
  mouseDictatedPlacement,
  nonDictatedPlacement,
  previousNodeDictatedPlacement,
  type Environment,
} from '@/components/ComponentBrowser/placement'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import GraphMouse from '@/components/GraphMouse.vue'
import PlusButton from '@/components/PlusButton.vue'
import TopBar from '@/components/TopBar.vue'
import { useDoubleClick } from '@/composables/doubleClick'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/composables/events'
import { useStackNavigator } from '@/composables/stackNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { colorFromString } from '@/util/colors'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import * as set from 'lib0/set'
import type { ExprId, NodeMetadata } from 'shared/yjsModel'
import { computed, onMounted, onUnmounted, ref, watch } from 'vue'
import { toast } from 'vue3-toastify'
import { type Usage } from './ComponentBrowser/input'

const EXECUTION_MODES = ['design', 'live']
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
const componentBrowserNodePosition = ref<Vec2>(Vec2.Zero)
const componentBrowserUsage = ref<Usage>({ type: 'newNode' })
const suggestionDb = useSuggestionDbStore()
const interaction = provideInteractionHandler()

function initStartupToast() {
  const startupToast = toast.info('Initializing the project. This can take up to one minute.', {
    autoClose: false,
  })
  projectStore.firstExecution.then(() => {
    if (startupToast != null) {
      toast.remove(startupToast)
    }
  })
  onUnmounted(() => {
    if (startupToast != null) {
      toast.remove(startupToast)
    }
  })
}

onMounted(() => {
  initStartupToast()
})

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
function targetComponentBrowserNodePosition() {
  const editedInfo = graphStore.editedNodeInfo
  const isEditingNode = editedInfo != null
  if (isEditingNode) {
    const targetNode = graphStore.db.nodeIdToNode.get(editedInfo.id)
    return targetNode?.position ?? Vec2.Zero
  } else {
    return (
      placementPositionForSelection() ??
      mouseDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value).position
    )
  }
}

function sourcePortForSelection() {
  if (graphStore.editedNodeInfo != null) return undefined
  const firstSelectedNode = set.first(nodeSelection.selected)
  return graphStore.db.getNodeFirstOutputPort(firstSelectedNode)
}

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
      nodeSelection.selected.clear()
    })
  },
  zoomToSelected() {
    if (!viewportNode.value) return
    let left = Infinity
    let top = Infinity
    let right = -Infinity
    let bottom = -Infinity
    const nodesToCenter =
      nodeSelection.selected.size === 0 ? graphStore.currentNodeIds : nodeSelection.selected
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
  enterNode() {
    if (keyboardBusy()) return false
    const selectedNode = set.first(nodeSelection.selected)
    if (selectedNode) {
      stackNavigator.enterNode(selectedNode)
    }
  },
  exitNode() {
    if (keyboardBusy()) return false
    stackNavigator.exitNode()
  },
})

const handleClick = useDoubleClick(
  (e: MouseEvent) => {
    graphBindingsHandler(e)
  },
  () => {
    stackNavigator.exitNode()
  },
).handleClick
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
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})

const editingNode: Interaction = {
  init: () => {
    // component browser usage is set in `graphStore.editedNodeInfo` watch
    componentBrowserNodePosition.value = targetComponentBrowserNodePosition()
  },
  cancel: () => {
    hideComponentBrowser()
    graphStore.editedNodeInfo = undefined
  },
}
const nodeIsBeingEdited = computed(() => graphStore.editedNodeInfo != null)
interaction.setWhen(nodeIsBeingEdited, editingNode)

const creatingNode: Interaction = {
  init: () => {
    componentBrowserUsage.value = { type: 'newNode', sourcePort: sourcePortForSelection() }
    componentBrowserNodePosition.value = targetComponentBrowserNodePosition()
    componentBrowserVisible.value = true
  },
  cancel: hideComponentBrowser,
}

const creatingNodeFromButton: Interaction = {
  init: () => {
    componentBrowserUsage.value = { type: 'newNode', sourcePort: sourcePortForSelection() }
    let targetPos = placementPositionForSelection()
    if (targetPos == undefined) {
      targetPos = nonDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value).position
    }
    componentBrowserNodePosition.value = targetPos
    componentBrowserVisible.value = true
  },
  cancel: hideComponentBrowser,
}

const creatingNodeFromPortDoubleClick: Interaction = {
  init: () => {
    // component browser usage is set in event handler
    componentBrowserVisible.value = true
  },
  cancel: hideComponentBrowser,
}

const creatingNodeFromEdgeDrop: Interaction = {
  init: () => {
    // component browser usage is set in event handler
    componentBrowserVisible.value = true
  },
  cancel: hideComponentBrowser,
}

function hideComponentBrowser() {
  componentBrowserVisible.value = false
}

function onComponentBrowserCommit(content: string, requiredImports: RequiredImport[]) {
  if (content != null) {
    if (graphStore.editedNodeInfo) {
      // We finish editing a node.
      graphStore.setNodeContent(graphStore.editedNodeInfo.id, content)
    } else {
      // We finish creating a new node.
      const metadata = undefined
      graphStore.createNode(componentBrowserNodePosition.value, content, metadata, requiredImports)
    }
  }
  // Finish interaction. This should also hide component browser.
  interaction.setCurrent(undefined)
}

function onComponentBrowserCancel() {
  // Finish interaction. This should also hide component browser.
  interaction.setCurrent(undefined)
}

// Watch the `editedNode` in the graph store
watch(
  () => graphStore.editedNodeInfo,
  (editedInfo) => {
    if (editedInfo) {
      componentBrowserNodePosition.value = targetComponentBrowserNodePosition()
      componentBrowserUsage.value = {
        type: 'editNode',
        node: editedInfo.id,
        cursorPos: editedInfo.initialCursorPos,
      }
      componentBrowserVisible.value = true
    } else {
      componentBrowserVisible.value = false
    }
  },
)

async function handleFileDrop(event: DragEvent) {
  // A vertical gap between created nodes when multiple files were dropped together.
  const MULTIPLE_FILES_GAP = 50

  if (!event.dataTransfer?.items) return
  ;[...event.dataTransfer.items].forEach(async (item, index) => {
    try {
      if (item.kind === 'file') {
        const file = item.getAsFile()
        if (!file) return
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
          projectStore.isOnLocalBackend,
          event.shiftKey,
          projectStore.executionContext.getStackTop(),
        )
        const uploadResult = await uploader.upload()
        graphStore.createNode(pos, uploadedExpression(uploadResult))
      }
    } catch (err) {
      console.error(`Uploading file failed. ${err}`)
    }
  })
}

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
  const content = node.rootSpan.code()
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

      if (type === 'text/html') {
        const blob = await clipboardItem.getType(type)
        const htmlContent = await blob.text()
        const excelPayload = await readNodeFromExcelClipboard(htmlContent, clipboardItem)
        if (excelPayload) {
          return excelPayload
        }
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

async function readNodeFromExcelClipboard(
  htmlContent: string,
  clipboardItem: ClipboardItem,
): Promise<ClipboardData | undefined> {
  // Check we have a valid HTML table
  // If it is Excel, we should have a plain-text version of the table with tab separators.
  if (
    clipboardItem.types.includes('text/plain') &&
    htmlContent.startsWith('<table ') &&
    htmlContent.endsWith('</table>')
  ) {
    const textData = await clipboardItem.getType('text/plain')
    const text = await textData.text()
    const payload = JSON.stringify(text).replaceAll(/^"|"$/g, '').replaceAll("'", "\\'")
    const expression = `'${payload}'.to Table`
    return { nodes: [{ expression: expression, metadata: undefined }] } as ClipboardData
  }
  return undefined
}

function handleNodeOutputPortDoubleClick(id: ExprId) {
  componentBrowserUsage.value = { type: 'newNode', sourcePort: id }
  const placementEnvironment = environmentForNodes([id].values())
  componentBrowserNodePosition.value = previousNodeDictatedPlacement(
    DEFAULT_NODE_SIZE,
    placementEnvironment,
    {
      horizontalGap: gapBetweenNodes,
      verticalGap: gapBetweenNodes,
    },
  ).position
  interaction.setCurrent(creatingNodeFromPortDoubleClick)
}

const stackNavigator = useStackNavigator()

function handleEdgeDrop(source: ExprId, position: Vec2) {
  componentBrowserUsage.value = { type: 'newNode', sourcePort: source }
  componentBrowserNodePosition.value = position
  interaction.setCurrent(creatingNodeFromEdgeDrop)
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
    @click="handleClick"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <ToastContainer
      position="top-center"
      theme="light"
      closeOnClick="false"
      draggable="false"
      toastClassName="text-sm leading-170 bg-frame-selected rounded-2xl backdrop-blur-3xl"
      transition="Vue-Toastification__bounce"
    />
    <div :style="{ transform: graphNavigator.transform }" class="htmlLayer">
      <GraphNodes
        @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick"
        @nodeDoubleClick="(id) => stackNavigator.enterNode(id)"
      />
    </div>
    <svg :viewBox="graphNavigator.viewBox" class="svgBackdropLayer">
      <GraphEdges @createNodeFromEdge="handleEdgeDrop" />
    </svg>

    <ComponentBrowser
      v-if="componentBrowserVisible"
      ref="componentBrowser"
      :navigator="graphNavigator"
      :nodePosition="componentBrowserNodePosition"
      :usage="componentBrowserUsage"
      @accepted="onComponentBrowserCommit"
      @closed="onComponentBrowserCommit"
      @canceled="onComponentBrowserCancel"
    />
    <TopBar
      v-model:mode="projectStore.executionMode"
      :title="projectStore.displayName"
      :modes="EXECUTION_MODES"
      :breadcrumbs="stackNavigator.breadcrumbLabels.value"
      :allowNavigationLeft="stackNavigator.allowNavigationLeft.value"
      :allowNavigationRight="stackNavigator.allowNavigationRight.value"
      @breadcrumbClick="stackNavigator.handleBreadcrumbClick"
      @back="stackNavigator.exitNode"
      @forward="stackNavigator.enterNextNodeFromHistory"
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
  --group-color-fallback: #006b8a;
  --node-color-no-type: #596b81;
}

.svgBackdropLayer {
  position: absolute;
  top: 0;
  left: 0;
  z-index: -1;
}

.htmlLayer {
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 0;
}
</style>
