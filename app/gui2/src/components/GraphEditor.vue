<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { type Usage } from '@/components/ComponentBrowser/input'
import { usePlacement } from '@/components/ComponentBrowser/placement'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { useGraphEditorClipboard } from '@/components/GraphEditor/clipboard'
import { performCollapse, prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { useGraphEditorToasts } from '@/components/GraphEditor/toasts'
import { Uploader, uploadedExpression } from '@/components/GraphEditor/upload'
import GraphMouse from '@/components/GraphMouse.vue'
import PlusButton from '@/components/PlusButton.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import TopBar from '@/components/TopBar.vue'
import { useDoubleClick } from '@/composables/doubleClick'
import { keyboardBusy, keyboardBusyExceptIn, useEvent } from '@/composables/events'
import { useNavigatorStorage } from '@/composables/navigatorStorage'
import type { PlacementStrategy } from '@/composables/nodeCreation'
import { useStackNavigator } from '@/composables/stackNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideNodeColors } from '@/providers/graphNodeColors'
import { provideNodeCreation } from '@/providers/graphNodeCreation'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideKeyboard } from '@/providers/keyboard'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import { useGraphStore, type NodeId } from '@/stores/graph'
import type { RequiredImport } from '@/stores/graph/imports'
import { useProjectStore } from '@/stores/project'
import { groupColorVar, useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { bail } from '@/util/assert'
import type { AstId } from '@/util/ast/abstract'
import { colorFromString } from '@/util/colors'
import { partition } from '@/util/data/array'
import { filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { encoding, set } from 'lib0'
import { encodeMethodPointer } from 'shared/languageServerTypes'
import { computed, onMounted, ref, shallowRef, toRef, watch } from 'vue'

const keyboard = provideKeyboard()
const graphStore = useGraphStore()
const widgetRegistry = provideWidgetRegistry(graphStore.db)
widgetRegistry.loadBuiltins()
const projectStore = useProjectStore()
const suggestionDb = useSuggestionDbStore()

// === Navigator ===

const viewportNode = ref<HTMLElement>()
onMounted(() => viewportNode.value?.focus())
const graphNavigator = provideGraphNavigator(viewportNode, keyboard)
useNavigatorStorage(graphNavigator, (enc) => {
  // Navigator viewport needs to be stored separately for:
  // - each project
  // - each function within the project
  encoding.writeVarString(enc, projectStore.name)
  const methodPtr = graphStore.currentMethodPointer()
  if (methodPtr != null) encodeMethodPointer(enc, methodPtr)
})

function selectionBounds() {
  if (!viewportNode.value) return
  const allNodes = graphStore.db.nodeIdToNode
  const validSelected = [...nodeSelection.selected].filter((id) => allNodes.has(id))
  const nodesToCenter = validSelected.length === 0 ? allNodes.keys() : validSelected
  let bounds = Rect.Bounding()
  for (const id of nodesToCenter) {
    const rect = graphStore.visibleArea(id)
    if (rect) bounds = Rect.Bounding(bounds, rect)
  }
  if (bounds.isFinite()) return bounds
}

function zoomToSelected() {
  const bounds = selectionBounds()
  if (bounds) graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale))
}

function panToSelected() {
  const bounds = selectionBounds()
  if (bounds)
    graphNavigator.panTo([new Vec2(bounds.left, bounds.top), new Vec2(bounds.right, bounds.bottom)])
}

// == Breadcrumbs ==

const stackNavigator = useStackNavigator()

// === Toasts ===

const toasts = useGraphEditorToasts()

// === Selection ===

const graphNodeSelections = shallowRef<HTMLElement>()
const nodeSelection = provideGraphSelection(
  graphNavigator,
  graphStore.nodeRects,
  graphStore.isPortEnabled,
  {
    onSelected(id) {
      graphStore.db.moveNodeToTop(id)
    },
  },
)

// Clear selection whenever the graph view is switched.
watch(
  () => projectStore.executionContext.getStackTop(),
  () => nodeSelection.deselectAll(),
)

// === Node creation ===

const { place: nodePlacement, collapse: collapsedNodePlacement } = usePlacement(
  toRef(graphStore, 'visibleNodeAreas'),
  toRef(graphNavigator, 'viewport'),
)

const { createNode, createNodes, placeNode } = provideNodeCreation(
  toRef(graphNavigator, 'viewport'),
  toRef(graphNavigator, 'sceneMousePos'),
  (nodes) => {
    clearFocus()
    nodeSelection.setSelection(nodes)
    panToSelected()
  },
)

// === Clipboard Copy/Paste ===

const { copySelectionToClipboard, createNodesFromClipboard } = useGraphEditorClipboard(
  nodeSelection,
  createNodes,
)

// === Interactions ===

const interaction = provideInteractionHandler()
const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
})

useEvent(window, 'keydown', (event) => {
  interactionBindingsHandler(event) ||
    (!keyboardBusy() && graphBindingsHandler(event)) ||
    (!keyboardBusyExceptIn(codeEditorArea.value) && codeEditorHandler(event))
})
useEvent(
  window,
  'pointerdown',
  (e) => interaction.handlePointerEvent(e, 'pointerdown', graphNavigator),
  {
    capture: true,
  },
)

useEvent(
  window,
  'pointerup',
  (e) => interaction.handlePointerEvent(e, 'pointerup', graphNavigator),
  {
    capture: true,
  },
)

// === Keyboard/Mouse bindings ===

const graphBindingsHandler = graphBindings.handler({
  undo() {
    projectStore.module?.undoManager.undo()
  },
  redo() {
    projectStore.module?.undoManager.redo()
  },
  startProfiling() {
    projectStore.lsRpcConnection.profilingStart(true)
  },
  stopProfiling() {
    projectStore.lsRpcConnection.profilingStop()
  },
  openComponentBrowser() {
    if (keyboardBusy()) return false
    if (graphNavigator.sceneMousePos != null && !componentBrowserVisible.value) {
      createWithComponentBrowser(fromSelection() ?? { placement: { type: 'mouse' } })
    }
  },
  deleteSelected,
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
    copySelectionToClipboard()
  },
  pasteNode() {
    if (keyboardBusy()) return false
    createNodesFromClipboard()
  },
  collapse() {
    if (keyboardBusy()) return false
    collapseNodes()
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
  changeColorSelectedNodes() {
    showColorPicker.value = true
  },
})

const { handleClick } = useDoubleClick(
  (e: MouseEvent) => {
    if (e.target !== viewportNode.value) return false
    clearFocus()
  },
  (e: MouseEvent) => {
    if (e.target !== viewportNode.value) return false
    stackNavigator.exitNode()
  },
)

function deleteSelected() {
  graphStore.transact(() => {
    graphStore.deleteNodes([...nodeSelection.selected])
    nodeSelection.selected.clear()
  })
}

// === Code Editor ===

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

// === Execution Mode ===

/** Handle record-once button presses. */
function onRecordOnceButtonPress() {
  projectStore.lsRpcConnection.initialized.then(async () => {
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
    projectStore.executionContext.executionEnvironment = modeValue === 'live' ? 'Live' : 'Design'
  },
)

// === Component Browser ===

const componentBrowserVisible = ref(false)
const componentBrowserNodePosition = ref<Vec2>(Vec2.Zero)
const componentBrowserUsage = ref<Usage>({ type: 'newNode' })

function openComponentBrowser(usage: Usage, position: Vec2) {
  componentBrowserUsage.value = usage
  componentBrowserNodePosition.value = position
  componentBrowserVisible.value = true
}

function hideComponentBrowser() {
  graphStore.editedNodeInfo = undefined
  componentBrowserVisible.value = false
}

function editWithComponentBrowser(node: NodeId, cursorPos: number) {
  openComponentBrowser(
    { type: 'editNode', node, cursorPos },
    graphStore.db.nodeIdToNode.get(node)?.position ?? Vec2.Zero,
  )
}

function createWithComponentBrowser(options: NewNodeOptions) {
  openComponentBrowser(
    { type: 'newNode', sourcePort: options.sourcePort },
    placeNode(options.placement, nodePlacement),
  )
}

function commitComponentBrowser(content: string, requiredImports: RequiredImport[]) {
  if (graphStore.editedNodeInfo) {
    // We finish editing a node.
    graphStore.setNodeContent(graphStore.editedNodeInfo.id, content, requiredImports)
  } else if (content != '') {
    // We finish creating a new node.
    createNode(
      { type: 'fixed', position: componentBrowserNodePosition.value },
      content,
      undefined,
      undefined,
      requiredImports,
    )
  }
  hideComponentBrowser()
}

// Watch the `editedNode` in the graph store and synchronize component browser display with it.
watch(
  () => graphStore.editedNodeInfo,
  (editedInfo) => {
    if (editedInfo) {
      editWithComponentBrowser(editedInfo.id, editedInfo.initialCursorPos)
    } else {
      hideComponentBrowser()
    }
  },
)

// === Node Creation ===

interface NewNodeOptions {
  placement: PlacementStrategy
  sourcePort?: AstId | undefined
}

/**
 * Start creating a node, basing its inputs and position on the current selection, if any;
 * or the current viewport, otherwise.
 */
function addNodeAuto() {
  createWithComponentBrowser(fromSelection() ?? { placement: { type: 'viewport' } })
}

function fromSelection(): NewNodeOptions | undefined {
  if (graphStore.editedNodeInfo != null) return undefined
  const firstSelectedNode = set.first(nodeSelection.selected)
  return {
    placement: { type: 'source', node: firstSelectedNode },
    sourcePort: graphStore.db.getNodeFirstOutputPort(firstSelectedNode),
  }
}

function clearFocus() {
  if (
    document.activeElement instanceof HTMLElement ||
    document.activeElement instanceof SVGElement
  ) {
    document.activeElement.blur()
  }
}

function createNodesFromSource(sourceNode: NodeId, options: NodeCreationOptions[]) {
  const sourcePort = graphStore.db.getNodeFirstOutputPort(sourceNode)
  const sourcePortAst = graphStore.viewModule.get(sourcePort)
  const [toCommit, toEdit] = partition(options, (opts) => opts.commit)
  createNodes(
    toCommit.map((options: NodeCreationOptions) => ({
      placement: { type: 'source', node: sourceNode },
      expression: options.content!.instantiateCopied([sourcePortAst]).code(),
    })),
  )
  if (toEdit.length)
    createWithComponentBrowser({ placement: { type: 'source', node: sourceNode }, sourcePort })
}

function handleNodeOutputPortDoubleClick(id: AstId) {
  const srcNode = graphStore.db.getPatternExpressionNodeId(id)
  if (srcNode == null) {
    console.error('Impossible happened: Double click on port not belonging to any node: ', id)
    return
  }
  createWithComponentBrowser({ placement: { type: 'source', node: srcNode }, sourcePort: id })
}

function handleEdgeDrop(source: AstId, position: Vec2) {
  createWithComponentBrowser({ placement: { type: 'fixed', position }, sourcePort: source })
}

// === Node Collapsing ===

function collapseNodes() {
  const selected = nodeSelection.selected
  if (selected.size == 0) return
  try {
    const info = prepareCollapsedInfo(selected, graphStore.db)
    if (!info.ok) {
      toasts.userActionFailed.show(`Unable to group nodes: ${info.error.payload}.`)
      return
    }
    const currentMethod = projectStore.executionContext.getStackTop()
    const currentMethodName = graphStore.db.stackItemToMethodName(currentMethod)
    if (currentMethodName == null) {
      bail(`Cannot get the method name for the current execution stack item. ${currentMethod}`)
    }
    const topLevel = graphStore.topLevel
    if (!topLevel) {
      bail('BUG: no top level, collapsing not possible.')
    }
    const selectedNodeRects = filterDefined(Array.from(selected, graphStore.visibleArea))
    graphStore.edit((edit) => {
      const { refactoredNodeId, collapsedNodeIds, outputNodeId } = performCollapse(
        info.value,
        edit.getVersion(topLevel),
        graphStore.db,
        currentMethodName,
      )
      const position = collapsedNodePlacement(selectedNodeRects)
      edit.get(refactoredNodeId).mutableNodeMetadata().set('position', position.xy())
      if (outputNodeId != null) {
        const collapsedNodeRects = filterDefined(
          Array.from(collapsedNodeIds, graphStore.visibleArea),
        )
        const { place } = usePlacement(collapsedNodeRects, graphNavigator.viewport)
        const position = place(collapsedNodeRects)
        edit.get(outputNodeId).mutableNodeMetadata().set('position', position.xy())
      }
    })
  } catch (err) {
    console.log('Error while collapsing, this is not normal.', err)
  }
}

// === Drag and drop ===

async function handleFileDrop(event: DragEvent) {
  // A vertical gap between created nodes when multiple files were dropped together.
  const MULTIPLE_FILES_GAP = 50

  if (!event.dataTransfer?.items) return
  ;[...event.dataTransfer.items].forEach(async (item, index) => {
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
      if (uploadResult.ok) {
        createNode({ type: 'mouseEvent', position: pos }, uploadedExpression(uploadResult.value))
      } else {
        uploadResult.error.log(`Uploading file failed`)
      }
    }
  })
}

// === Color Picker ===

provideNodeColors((variable) =>
  viewportNode.value ? getComputedStyle(viewportNode.value).getPropertyValue(variable) : '',
)

const showColorPicker = ref(false)

function setSelectedNodesColor(color: string) {
  graphStore.transact(() =>
    nodeSelection.selected.forEach((id) => graphStore.overrideNodeColor(id, color)),
  )
}

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (let group of suggestionDb.groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})
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
        :graphNodeSelections="graphNodeSelections"
        @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick"
        @nodeDoubleClick="(id) => stackNavigator.enterNode(id)"
        @createNodes="createNodesFromSource"
        @setNodeColor="setSelectedNodesColor"
      />
    </div>
    <div
      ref="graphNodeSelections"
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
      v-model:showColorPicker="showColorPicker"
      :breadcrumbs="stackNavigator.breadcrumbLabels.value"
      :allowNavigationLeft="stackNavigator.allowNavigationLeft.value"
      :allowNavigationRight="stackNavigator.allowNavigationRight.value"
      :zoomLevel="100.0 * graphNavigator.targetScale"
      :componentsSelected="nodeSelection.selected.size"
      @breadcrumbClick="stackNavigator.handleBreadcrumbClick"
      @back="stackNavigator.exitNode"
      @forward="stackNavigator.enterNextNodeFromHistory"
      @recordOnce="onRecordOnceButtonPress()"
      @fitToAllClicked="zoomToSelected"
      @zoomIn="graphNavigator.stepZoom(+1)"
      @zoomOut="graphNavigator.stepZoom(-1)"
      @toggleCodeEditor="toggleCodeEditor"
      @collapseNodes="collapseNodes"
      @setNodeColor="setSelectedNodesColor"
      @removeNodes="deleteSelected"
    />
    <PlusButton @click.stop="addNodeAuto()" />
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
