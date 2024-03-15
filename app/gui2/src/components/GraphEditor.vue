<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import {
  collapsedNodePlacement,
  mouseDictatedPlacement,
  nonDictatedPlacement,
  previousNodeDictatedPlacement,
  type Environment,
} from '@/components/ComponentBrowser/placement'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { performCollapse, prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import GraphMouse from '@/components/GraphMouse.vue'
import PlusButton from '@/components/PlusButton.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import TopBar from '@/components/TopBar.vue'
import { useDoubleClick } from '@/composables/doubleClick'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/composables/events'
import { useStackNavigator } from '@/composables/stackNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideKeyboard } from '@/providers/keyboard'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { bail } from '@/util/assert'
import type { AstId, NodeMetadataFields } from '@/util/ast/abstract'
import { colorFromString } from '@/util/colors'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import * as set from 'lib0/set'
import { computed, onMounted, ref, watch } from 'vue'
import { ProjectManagerEvents } from '../../../ide-desktop/lib/dashboard/src/utilities/ProjectManager'
import { type Usage } from './ComponentBrowser/input'

// Assumed size of a newly created node. This is used to place the component browser.
const DEFAULT_NODE_SIZE = new Vec2(0, 24)
const gapBetweenNodes = 48.0

const keyboard = provideKeyboard()
const viewportNode = ref<HTMLElement>()
const graphNavigator = provideGraphNavigator(viewportNode, keyboard)
const graphStore = useGraphStore()
const widgetRegistry = provideWidgetRegistry(graphStore.db)
widgetRegistry.loadBuiltins()
const projectStore = useProjectStore()
const componentBrowserVisible = ref(false)
const componentBrowserNodePosition = ref<Vec2>(Vec2.Zero)
const componentBrowserUsage = ref<Usage>({ type: 'newNode' })
const suggestionDb = useSuggestionDbStore()
const interaction = provideInteractionHandler()

// === toasts ===

const toastStartup = useToast.info({ autoClose: false })
const toastConnectionLost = useToast.error({ autoClose: false })
const toastLspError = useToast.error()
const toastConnectionError = useToast.error()
const toastExecutionFailed = useToast.error()

toastStartup.show('Initializing the project. This can take up to one minute.')
projectStore.firstExecution.then(toastStartup.dismiss)

useEvent(document, ProjectManagerEvents.loadingFailed, () =>
  toastConnectionLost.show('Lost connection to Language Server.'),
)

projectStore.lsRpcConnection.then(
  (ls) => ls.client.onError((e) => toastLspError.show(`Language server error: ${e}`)),
  (e) => toastConnectionError.show(`Connection to language server failed: ${JSON.stringify(e)}`),
)

projectStore.executionContext.on('executionComplete', () => toastExecutionFailed.dismiss())
projectStore.executionContext.on('executionFailed', (e) =>
  toastExecutionFailed.show(`Execution Failed: ${JSON.stringify(e)}`),
)

// === nodes ===

const nodeSelection = provideGraphSelection(graphNavigator, graphStore.nodeRects, {
  onSelected(id) {
    graphStore.db.moveNodeToTop(id)
  },
})

const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
})

// Return the environment for the placement of a new node. The passed nodes should be the nodes that are
// used as the source of the placement. This means, for example, the selected nodes when creating from a selection
// or the node that is being edited when creating from a port double click.
function environmentForNodes(nodeIds: IterableIterator<NodeId>): Environment {
  const nodeRects = graphStore.visibleNodeAreas
  const selectedNodeRects = [...nodeIds]
    .map((id) => graphStore.vizRects.get(id) ?? graphStore.nodeRects.get(id))
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
  interactionBindingsHandler(event) ||
    (!keyboardBusy() && graphBindingsHandler(event)) ||
    (!keyboardBusyExceptIn(codeEditorArea.value) && codeEditorHandler(event))
})
useEvent(window, 'pointerdown', (e) => interaction.handleClick(e, graphNavigator), {
  capture: true,
})

onMounted(() => viewportNode.value?.focus())

function zoomToSelected() {
  if (!viewportNode.value) return
  const nodesToCenter =
    nodeSelection.selected.size === 0 ? graphStore.db.nodeIdToNode.keys() : nodeSelection.selected
  let bounds = Rect.Bounding()
  for (const id of nodesToCenter) {
    const rect = graphStore.vizRects.get(id) ?? graphStore.nodeRects.get(id)
    if (rect) bounds = Rect.Bounding(bounds, rect)
  }
  graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.scale))
}

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
      showComponentBrowser()
    }
  },
  deleteSelected() {
    graphStore.transact(() => {
      graphStore.deleteNodes([...nodeSelection.selected])
      nodeSelection.selected.clear()
    })
  },
  zoomToSelected() {
    zoomToSelected()
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
    graphStore.transact(() => {
      const allVisible = set
        .toArray(nodeSelection.selected)
        .every((id) => !(graphStore.db.nodeIdToNode.get(id)?.vis?.visible !== true))

      for (const nodeId of nodeSelection.selected) {
        graphStore.setNodeVisualization(nodeId, { visible: !allVisible })
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
  collapse() {
    if (keyboardBusy()) return false
    const selected = new Set(nodeSelection.selected)
    if (selected.size == 0) return
    try {
      const info = prepareCollapsedInfo(selected, graphStore.db)
      const currentMethod = projectStore.executionContext.getStackTop()
      const currentMethodName = graphStore.db.stackItemToMethodName(currentMethod)
      if (currentMethodName == null) {
        bail(`Cannot get the method name for the current execution stack item. ${currentMethod}`)
      }
      const currentFunctionEnv = environmentForNodes(selected.values())
      const topLevel = graphStore.topLevel
      if (!topLevel) {
        bail('BUG: no top level, collapsing not possible.')
      }
      const { position } = collapsedNodePlacement(DEFAULT_NODE_SIZE, currentFunctionEnv)
      graphStore.edit((edit) => {
        const { refactoredNodeId, collapsedNodeIds, outputNodeId } = performCollapse(
          info,
          edit.getVersion(topLevel),
          graphStore.db,
          currentMethodName,
        )
        const collapsedFunctionEnv = environmentForNodes(collapsedNodeIds.values())
        // For collapsed function, only selected nodes would affect placement of the output node.
        collapsedFunctionEnv.nodeRects = collapsedFunctionEnv.selectedNodeRects
        edit
          .get(refactoredNodeId)
          .mutableNodeMetadata()
          .set('position', { x: position.x, y: position.y })
        if (outputNodeId != null) {
          const { position } = previousNodeDictatedPlacement(
            DEFAULT_NODE_SIZE,
            collapsedFunctionEnv,
          )
          edit
            .get(outputNodeId)
            .mutableNodeMetadata()
            .set('position', { x: position.x, y: position.y })
        }
      })
    } catch (err) {
      console.log('Error while collapsing, this is not normal.', err)
    }
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

const { handleClick } = useDoubleClick(
  (e: MouseEvent) => {
    graphBindingsHandler(e)
    if (document.activeElement instanceof HTMLElement) {
      document.activeElement.blur()
    }
  },
  () => {
    stackNavigator.exitNode()
  },
)
const codeEditorArea = ref<HTMLElement>()
const showCodeEditor = ref(false)
const toggleCodeEditor = () => {
  showCodeEditor.value = !showCodeEditor.value
}
const codeEditorHandler = codeEditorBindings.handler({
  toggle() {
    toggleCodeEditor()
  },
})

/** Handle record-once button presses. */
function onRecordOnceButtonPress() {
  projectStore.lsRpcConnection.then(async () => {
    const modeValue = projectStore.executionMode
    if (modeValue == undefined) {
      return
    }
    projectStore.executionContext.recompute('all', 'Live')
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

function showComponentBrowser(nodePosition?: Vec2 | undefined, usage?: Usage) {
  componentBrowserUsage.value = usage ?? { type: 'newNode', sourcePort: sourcePortForSelection() }
  componentBrowserNodePosition.value = nodePosition ?? targetComponentBrowserNodePosition()
  componentBrowserVisible.value = true
}

/** Start creating a node, basing its inputs and position on the current selection, if any;
 *  or the current viewport, otherwise.
 */
function addNodeAuto() {
  const targetPos =
    placementPositionForSelection() ??
    nonDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment.value).position
  showComponentBrowser(targetPos)
}

function addNodeAt(sourceNode: NodeId, pos: Vec2 | undefined) {
  const sourcePort = graphStore.db.getNodeFirstOutputPort(sourceNode)
  showComponentBrowser(pos, { type: 'newNode', sourcePort })
}

function hideComponentBrowser() {
  graphStore.editedNodeInfo = undefined
  componentBrowserVisible.value = false
}

function commitComponentBrowser(content: string, requiredImports: RequiredImport[]) {
  if (content != null) {
    if (graphStore.editedNodeInfo) {
      // We finish editing a node.
      graphStore.setNodeContent(graphStore.editedNodeInfo.id, content, requiredImports)
    } else if (content != '') {
      // We finish creating a new node.
      const metadata = undefined
      const createdNode = graphStore.createNode(
        componentBrowserNodePosition.value,
        content,
        metadata,
        requiredImports,
      )
      if (createdNode) nodeSelection.setSelection(new Set([createdNode]))
    }
  }
  hideComponentBrowser()
}

// Watch the `editedNode` in the graph store
watch(
  () => graphStore.editedNodeInfo,
  (editedInfo) => {
    if (editedInfo) {
      showComponentBrowser(undefined, {
        type: 'editNode',
        node: editedInfo.id,
        cursorPos: editedInfo.initialCursorPos,
      })
    } else {
      hideComponentBrowser()
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
  metadata: NodeMetadataFields | undefined
}

/** Copy the content of the selected node to the clipboard. */
function copyNodeContent() {
  const id = nodeSelection.selected.values().next().value
  const node = graphStore.db.nodeIdToNode.get(id)
  if (!node) return
  const content = node.innerExpr.code()
  const nodeMetadata = node.rootExpr.nodeMetadata
  const metadata = {
    position: nodeMetadata.get('position'),
    visualization: nodeMetadata.get('visualization'),
  }
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

function handleNodeOutputPortDoubleClick(id: AstId) {
  const srcNode = graphStore.db.getPatternExpressionNodeId(id)
  if (srcNode == null) {
    console.error('Impossible happened: Double click on port not belonging to any node: ', id)
    return
  }
  const placementEnvironment = environmentForNodes([srcNode].values())
  const position = previousNodeDictatedPlacement(DEFAULT_NODE_SIZE, placementEnvironment, {
    horizontalGap: gapBetweenNodes,
    verticalGap: gapBetweenNodes,
  }).position
  showComponentBrowser(position, { type: 'newNode', sourcePort: id })
}

const stackNavigator = useStackNavigator()

function handleEdgeDrop(source: AstId, position: Vec2) {
  showComponentBrowser(position, { type: 'newNode', sourcePort: source })
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
    <div class="layer" :style="{ transform: graphNavigator.transform }">
      <GraphNodes
        @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick"
        @nodeDoubleClick="(id) => stackNavigator.enterNode(id)"
        @addNode="addNodeAt"
      />
    </div>
    <div
      id="graphNodeSelections"
      class="layer"
      :style="{ transform: graphNavigator.transform, 'z-index': -1 }"
    />
    <GraphEdges :navigator="graphNavigator" @createNodeFromEdge="handleEdgeDrop" />

    <ComponentBrowser
      v-if="componentBrowserVisible"
      ref="componentBrowser"
      :navigator="graphNavigator"
      :nodePosition="componentBrowserNodePosition"
      :usage="componentBrowserUsage"
      @accepted="commitComponentBrowser"
      @canceled="hideComponentBrowser"
    />
    <TopBar
      v-model:recordMode="projectStore.recordMode"
      :breadcrumbs="stackNavigator.breadcrumbLabels.value"
      :allowNavigationLeft="stackNavigator.allowNavigationLeft.value"
      :allowNavigationRight="stackNavigator.allowNavigationRight.value"
      :zoomLevel="100.0 * graphNavigator.scale"
      @breadcrumbClick="stackNavigator.handleBreadcrumbClick"
      @back="stackNavigator.exitNode"
      @forward="stackNavigator.enterNextNodeFromHistory"
      @recordOnce="onRecordOnceButtonPress()"
      @fitToAllClicked="zoomToSelected"
      @zoomIn="graphNavigator.stepZoom(+1)"
      @zoomOut="graphNavigator.stepZoom(-1)"
      @toggleCodeEditor="toggleCodeEditor"
    />
    <PlusButton @pointerdown.stop @click.stop="addNodeAuto()" @pointerup.stop />
    <Transition>
      <Suspense ref="codeEditorArea">
        <CodeEditor v-if="showCodeEditor" @close="showCodeEditor = false" />
      </Suspense>
    </Transition>
    <SceneScroller
      :navigator="graphNavigator"
      :scrollableArea="Rect.Bounding(...graphStore.visibleNodeAreas)"
    />
    <GraphMouse />
  </div>
</template>

<style scoped>
.GraphEditor {
  position: relative;
  contain: layout;
  overflow: clip;
  user-select: none;
  --group-color-fallback: #006b8a;
  --node-color-no-type: #596b81;
}

.layer {
  position: absolute;
  top: 0;
  left: 0;
  width: 0;
  height: 0;
}
</style>
