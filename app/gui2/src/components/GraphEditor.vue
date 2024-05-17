<script setup lang="ts">
import {
  codeEditorBindings,
  documentationEditorBindings,
  graphBindings,
  interactionBindings,
  undoBindings,
} from '@/bindings'
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
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import PlusButton from '@/components/PlusButton.vue'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import TopBar from '@/components/TopBar.vue'
import { useAstDocumentation } from '@/composables/astDocumentation'
import { useDoubleClick } from '@/composables/doubleClick'
import {
  keyboardBusy,
  keyboardBusyExceptIn,
  useEvent,
  useResizeObserver,
} from '@/composables/events'
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
import { every, filterDefined } from '@/util/data/iterable'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { encoding, set } from 'lib0'
import { encodeMethodPointer } from 'shared/languageServerTypes'
import { computed, onMounted, ref, shallowRef, toRef, watch, watchEffect } from 'vue'

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
useNavigatorStorage(
  graphNavigator,
  (enc) => {
    // Navigator viewport needs to be stored separately for:
    // - each project
    // - each function within the project
    encoding.writeVarString(enc, projectStore.name)
    const methodPtr = graphStore.currentMethodPointer()
    if (methodPtr != null) encodeMethodPointer(enc, methodPtr)
  },
  waitInitializationAndPanToAll,
)

let stopInitialization: (() => void) | undefined
function waitInitializationAndPanToAll() {
  stopInitialization?.()
  stopInitialization = watchEffect(() => {
    const nodesCount = graphStore.db.nodeIdToNode.size
    const visibleNodeAreas = graphStore.visibleNodeAreas
    if (nodesCount > 0 && visibleNodeAreas.length == nodesCount) {
      zoomToSelected(true)
      stopInitialization?.()
      stopInitialization = undefined
    }
  })
}

function selectionBounds() {
  if (!viewportNode.value) return
  const selected = nodeSelection.selected
  const nodesToCenter = selected.size === 0 ? graphStore.db.nodeIdToNode.keys() : selected
  let bounds = Rect.Bounding()
  for (const id of nodesToCenter) {
    const rect = graphStore.visibleArea(id)
    if (rect) bounds = Rect.Bounding(bounds, rect)
  }
  if (bounds.isFinite()) return bounds
}

function zoomToSelected(skipAnimation: boolean = false) {
  const bounds = selectionBounds()
  if (bounds)
    graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale), skipAnimation)
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
  (id) => graphStore.db.nodeIdToNode.has(id),
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
  toRef(nodeSelection, 'selected'),
  createNodes,
)

// === Interactions ===

const interaction = provideInteractionHandler()
const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
})

useEvent(window, 'keydown', (event) => {
  interactionBindingsHandler(event) ||
    (!keyboardBusyExceptIn(documentationEditorArea.value) && undoBindingsHandler(event)) ||
    (!keyboardBusy() && graphBindingsHandler(event)) ||
    (!keyboardBusyExceptIn(codeEditorArea.value) && codeEditorHandler(event)) ||
    (!keyboardBusyExceptIn(documentationEditorArea.value) && documentationEditorHandler(event))
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

const undoBindingsHandler = undoBindings.handler({
  undo() {
    graphStore.undoManager.undo()
  },
  redo() {
    graphStore.undoManager.redo()
  },
})

const graphBindingsHandler = graphBindings.handler({
  startProfiling() {
    projectStore.lsRpcConnection.profilingStart(true)
  },
  stopProfiling() {
    projectStore.lsRpcConnection.profilingStop()
  },
  openComponentBrowser() {
    if (graphNavigator.sceneMousePos != null && !componentBrowserVisible.value) {
      createWithComponentBrowser(fromSelection() ?? { placement: { type: 'mouse' } })
    }
  },
  deleteSelected,
  zoomToSelected,
  selectAll() {
    nodeSelection.selectAll()
  },
  deselectAll() {
    nodeSelection.deselectAll()
    clearFocus()
    graphStore.undoManager.undoStackBoundary()
  },
  toggleVisualization() {
    const selected = nodeSelection.selected
    const allVisible = every(
      selected,
      (id) => graphStore.db.nodeIdToNode.get(id)?.vis?.visible === true,
    )
    graphStore.transact(() => {
      for (const nodeId of selected) {
        graphStore.setNodeVisualization(nodeId, { visible: !allVisible })
      }
    })
  },
  copyNode() {
    copySelectionToClipboard()
  },
  pasteNode() {
    createNodesFromClipboard()
  },
  collapse() {
    collapseNodes()
  },
  enterNode() {
    const selectedNode = set.first(nodeSelection.selected)
    if (selectedNode) {
      stackNavigator.enterNode(selectedNode)
    }
  },
  exitNode() {
    stackNavigator.exitNode()
  },
  changeColorSelectedNodes() {
    showColorPicker.value = true
  },
})

const { handleClick } = useDoubleClick(
  (e: MouseEvent) => {
    if (e.target !== e.currentTarget) return false
    clearFocus()
  },
  (e: MouseEvent) => {
    if (e.target !== e.currentTarget) return false
    stackNavigator.exitNode()
  },
)

function deleteSelected() {
  graphStore.deleteNodes(nodeSelection.selected)
  nodeSelection.deselectAll()
}

// === Code Editor ===

const codeEditorArea = ref<HTMLElement>()
const showCodeEditor = ref(false)
const codeEditorHandler = codeEditorBindings.handler({
  toggle() {
    showCodeEditor.value = !showCodeEditor.value
  },
})

// === Documentation Editor ===

const documentationEditorArea = ref<HTMLElement>()
const showDocumentationEditor = ref(false)

const documentationEditorHandler = documentationEditorBindings.handler({
  toggle() {
    showDocumentationEditor.value = !showDocumentationEditor.value
  },
})

const rightDockComputedSize = useResizeObserver(documentationEditorArea)
const rightDockComputedBounds = computed(() => new Rect(Vec2.Zero, rightDockComputedSize.value))
const rightDockWidth = ref<number>()
const cssRightDockWidth = computed(() =>
  rightDockWidth.value != null ? `${rightDockWidth.value}px` : 'var(--right-dock-default-width)',
)

const { documentation } = useAstDocumentation(() => graphStore.methodAst)

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
  if (firstSelectedNode == null) return undefined
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
    <Transition name="rightDock">
      <div
        v-if="showDocumentationEditor"
        ref="documentationEditorArea"
        class="rightDock"
        data-testid="rightDock"
      >
        <div class="scrollArea">
          <MarkdownEditor v-model="documentation" />
        </div>
        <SvgIcon
          name="close"
          class="closeButton button"
          @click.stop="showDocumentationEditor = false"
        />
        <ResizeHandles
          left
          :modelValue="rightDockComputedBounds"
          @update:modelValue="rightDockWidth = $event.width"
        />
      </div>
    </Transition>
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
      v-model:showCodeEditor="showCodeEditor"
      v-model:showDocumentationEditor="showDocumentationEditor"
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
      @collapseNodes="collapseNodes"
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
.rightDock {
  position: absolute;
  top: 46px;
  bottom: 0;
  width: v-bind('cssRightDockWidth');
  right: 0;
  border-radius: 7px 0 0;
  background-color: rgba(255, 255, 255, 0.35);
  backdrop-filter: var(--blur-app-bg);
  padding: 4px 12px 0 6px;
  /* Prevent absolutely-positioned children (such as the close button) from bypassing the show/hide animation. */
  overflow-x: clip;
}
.rightDock-enter-active,
.rightDock-leave-active {
  transition: left 0.25s ease;
}
.rightDock-enter-from,
.rightDock-leave-to {
  width: 0;
}
.rightDock .scrollArea {
  width: 100%;
  height: 100%;
  overflow-y: auto;
}

.rightDock .closeButton {
  position: absolute;
  top: 4px;
  right: 6px;
  color: red;
  opacity: 0.3;

  &:hover {
    opacity: 0.6;
  }
}

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
