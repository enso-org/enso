<script setup lang="ts">
import {
  codeEditorBindings,
  documentationEditorBindings,
  graphBindings,
  interactionBindings,
  undoBindings,
} from '@/bindings'
import BottomPanel from '@/components/BottomPanel.vue'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import type { Usage } from '@/components/ComponentBrowser/input'
import { usePlacement } from '@/components/ComponentBrowser/placement'
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { useGraphEditorClipboard } from '@/components/GraphEditor/clipboard'
import { performCollapse, prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { useGraphEditorToasts } from '@/components/GraphEditor/toasts'
import { uploadedExpression, Uploader } from '@/components/GraphEditor/upload'
import GraphMissingView from '@/components/GraphMissingView.vue'
import GraphMouse from '@/components/GraphMouse.vue'
import PlusButton from '@/components/PlusButton.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import TopBar from '@/components/TopBar.vue'
import { builtinWidgets } from '@/components/widgets'
import { useDoubleClick } from '@/composables/doubleClick'
import { keyboardBusy, keyboardBusyExceptIn, unrefElement, useEvent } from '@/composables/events'
import { groupColorVar } from '@/composables/nodeColors'
import type { PlacementStrategy } from '@/composables/nodeCreation'
import { provideGraphEditorLayers } from '@/providers/graphEditorLayers'
import { provideGraphEditorState } from '@/providers/graphEditorState'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideNodeColors } from '@/providers/graphNodeColors'
import { provideNodeCreation } from '@/providers/graphNodeCreation'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideStackNavigator } from '@/providers/graphStackNavigator'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideKeyboard } from '@/providers/keyboard'
import { provideSelectionButtons } from '@/providers/selectionButtons'
import { provideWidgetRegistry } from '@/providers/widgetRegistry'
import type { Node, NodeId } from '@/stores/graph'
import { provideGraphStore } from '@/stores/graph'
import { isInputNode, nodeId } from '@/stores/graph/graphDatabase'
import type { RequiredImport } from '@/stores/graph/imports'
import { providePersisted } from '@/stores/persisted'
import { useProjectStore } from '@/stores/project'
import { provideNodeExecution } from '@/stores/project/nodeExecution'
import { provideRightDock, StorageMode } from '@/stores/rightDock'
import { provideSuggestionDbStore } from '@/stores/suggestionDatabase'
import type { SuggestionId, Typename } from '@/stores/suggestionDatabase/entry'
import { suggestionDocumentationUrl } from '@/stores/suggestionDatabase/entry'
import { provideVisualizationStore } from '@/stores/visualization'
import { bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import { colorFromString } from '@/util/colors'
import { partition } from '@/util/data/array'
import { Rect } from '@/util/data/rect'
import { Err, Ok, unwrapOr } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import * as iter from 'enso-common/src/utilities/data/iter'
import { set } from 'lib0'
import {
  computed,
  onMounted,
  onUnmounted,
  ref,
  shallowRef,
  toRaw,
  toRef,
  watch,
  type ComponentInstance,
} from 'vue'
import RightDockPanel from './RightDockPanel.vue'

const keyboard = provideKeyboard()
const projectStore = useProjectStore()
const suggestionDb = provideSuggestionDbStore(projectStore)
const graphStore = provideGraphStore(projectStore, suggestionDb)
const widgetRegistry = provideWidgetRegistry(graphStore.db)
const _visualizationStore = provideVisualizationStore(projectStore)

provideNodeExecution(projectStore)
;(window as any)._mockSuggestion = suggestionDb.mockSuggestion

onMounted(() => {
  widgetRegistry.loadWidgets(Object.entries(builtinWidgets))
  if (import.meta.env.DEV) {
    ;(window as any).suggestionDb = toRaw(suggestionDb.entries)
  }
})
onUnmounted(() => {
  projectStore.disposeYDocsProvider()
})

// === Navigator ===

const viewportNode = ref<HTMLElement>()
onMounted(() => viewportNode.value?.focus())
const graphNavigator: GraphNavigator = provideGraphNavigator(viewportNode, keyboard, {
  predicate: (e) => (e instanceof KeyboardEvent ? nodeSelection.selected.size === 0 : true),
})

// === Exposed layers ===

const rootNode = ref<HTMLElement>()
const floatingLayer = ref<HTMLElement>()
provideGraphEditorLayers({
  fullscreen: rootNode,
  floating: floatingLayer,
})

// === Client saved state ===

const persisted = providePersisted(
  () => projectStore.id,
  graphStore,
  graphNavigator,
  () => zoomToAll(true),
)

const rightDock = provideRightDock(graphStore, persisted)

// === Zoom/pan ===

function nodesBounds(nodeIds: Iterable<NodeId>) {
  let bounds = Rect.Bounding()
  for (const id of nodeIds) {
    const rect = graphStore.visibleArea(id)
    if (rect) bounds = Rect.Bounding(bounds, rect)
  }
  if (bounds.isFinite()) return bounds
}

function selectionBounds() {
  const selected = nodeSelection.selected
  const nodesToCenter = selected.size === 0 ? graphStore.db.nodeIds() : selected
  return nodesBounds(nodesToCenter)
}

function zoomToSelected(skipAnimation: boolean = false) {
  const bounds = selectionBounds()
  if (bounds)
    graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale), skipAnimation)
}

function zoomToAll(skipAnimation: boolean = false) {
  const bounds = nodesBounds(graphStore.db.nodeIds())
  if (bounds)
    graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale), skipAnimation)
}

function panToSelected() {
  const bounds = selectionBounds()
  if (bounds)
    graphNavigator.panTo([new Vec2(bounds.left, bounds.top), new Vec2(bounds.right, bounds.bottom)])
}

// == Breadcrumbs ==

const stackNavigator = provideStackNavigator(projectStore, graphStore)
const graphMissing = computed(() => graphStore.moduleRoot != null && !graphStore.methodAst.ok)

// === Toasts ===

const toasts = useGraphEditorToasts(projectStore)

// === Selection ===

const nodeSelection = provideGraphSelection(
  graphNavigator,
  graphStore.nodeRects,
  graphStore.isPortEnabled,
  {
    isValid: (id) => graphStore.db.isNodeId(id),
    onSelected: (id) => graphStore.db.moveNodeToTop(id),
    toSorted: (ids) => {
      const idsSet = new Set(ids)
      const inputNodes = [
        ...iter.filter(
          iter.filterDefined(
            iter.map(idsSet, graphStore.db.nodeIdToNode.get.bind(graphStore.db.nodeIdToNode)),
          ),
          isInputNode,
        ),
      ]
      inputNodes.sort((a, b) => a.argIndex - b.argIndex)
      const nonInputNodeIds = graphStore.pickInCodeOrder(idsSet)
      return iter.chain(inputNodes.map(nodeId), nonInputNodeIds)
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

const { scheduleCreateNode, createNodes, placeNode } = provideNodeCreation(
  graphStore,
  toRef(graphNavigator, 'viewport'),
  toRef(graphNavigator, 'sceneMousePos'),
  (nodes) => {
    clearFocus()
    if (nodes.size > 0) {
      nodeSelection.setSelection(nodes)
      panToSelected()
    }
  },
)

// === Clipboard Copy/Paste ===

const { copyNodesToClipboard, createNodesFromClipboard } = useGraphEditorClipboard(createNodes)

// === Selection Buttons ===

const { buttons: selectionButtons } = provideSelectionButtons(
  () =>
    iter.filterDefined(
      iter.map(
        nodeSelection.selected,
        graphStore.db.nodeIdToNode.get.bind(graphStore.db.nodeIdToNode),
      ),
    ),
  {
    collapseNodes,
    copyNodesToClipboard,
    deleteNodes: (nodes) => graphStore.deleteNodes(nodes.map(nodeId)),
  },
)

// === Interactions ===

const interaction = provideInteractionHandler()
const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
})

useEvent(
  window,
  'keydown',
  (event) =>
    interactionBindingsHandler(event) ||
    (!keyboardBusy() && undoBindingsHandler(event)) ||
    (!keyboardBusy() && graphBindingsHandler(event)) ||
    (!keyboardBusyExceptIn(codeEditorArea.value) && codeEditorHandler(event)) ||
    (!keyboardBusyExceptIn(documentationEditorArea.value) && documentationEditorHandler(event)) ||
    (!keyboardBusy() && graphNavigator.keyboardEvents.keydown(event)),
)

useEvent(window, 'pointerdown', (e) => interaction.handlePointerEvent(e, 'pointerdown'), {
  capture: true,
})

useEvent(window, 'pointerup', (e) => interaction.handlePointerEvent(e, 'pointerup'), {
  capture: true,
})

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
    if (graphNavigator.sceneMousePos != null && !componentBrowserOpened.value) {
      createWithComponentBrowser(fromSelection() ?? { placement: { type: 'mouse' } })
    }
  },
  deleteSelected: selectionButtons.deleteSelected.action!,
  zoomToSelected() {
    zoomToSelected()
  },
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
    const allVisible = iter.every(
      selected,
      (id) => graphStore.db.nodeIdToNode.get(id)?.vis?.visible === true,
    )
    graphStore.batchEdits(() => {
      for (const nodeId of selected) {
        graphStore.setNodeVisualization(nodeId, { visible: !allVisible })
      }
    })
  },
  copyNode: selectionButtons.copy.action!,
  pasteNode() {
    createNodesFromClipboard()
  },
  collapse: selectionButtons.collapse.action!,
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
    selectionButtons.pickColorMulti.state = true
  },
  openDocumentation() {
    const result = tryGetSelectionDocUrl()
    if (!result.ok) {
      toasts.userActionFailed.show(result.error.message('Unable to show node documentation'))
      return
    }
    window.open(result.value, '_blank')
  },
})

function tryGetSelectionDocUrl() {
  const selected = nodeSelection.tryGetSoleSelection()
  if (!selected.ok) return selected
  const suggestion = graphStore.db.getNodeMainSuggestion(selected.value)
  const documentation = suggestion && suggestionDocumentationUrl(suggestion)
  if (!documentation) return Err('No external documentation available for selected component')
  return Ok(documentation)
}

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

// === Code Editor ===

const codeEditor = shallowRef<ComponentInstance<typeof CodeEditor>>()
const codeEditorArea = computed(() => unrefElement(codeEditor))
const showCodeEditor = ref(false)
const codeEditorHandler = codeEditorBindings.handler({
  toggle() {
    showCodeEditor.value = !showCodeEditor.value
  },
})

// === Documentation Editor ===

const displayedDocs = ref<SuggestionId>()
const aiMode = ref<boolean>(false)

function toggleRightDockHelpPanel() {
  rightDock.toggleVisible('help')
}

const docEditor = shallowRef<ComponentInstance<typeof DocumentationEditor>>()
const documentationEditorArea = computed(() => unrefElement(docEditor))

const documentationEditorHandler = documentationEditorBindings.handler({
  toggle: () => rightDock.toggleVisible(),
})

// === Component Browser ===

const { componentBrowserOpened } = provideGraphEditorState({
  componentBrowserOpened: ref(false),
})
const componentBrowserNodePosition = ref<Vec2>(Vec2.Zero)
const componentBrowserUsage = ref<Usage>({ type: 'newNode' })

watch(componentBrowserOpened, (v) =>
  rightDock.setStorageMode(v ? StorageMode.ComponentBrowser : StorageMode.Default),
)

function openComponentBrowser(usage: Usage, position: Vec2) {
  componentBrowserUsage.value = usage
  componentBrowserNodePosition.value = position
  componentBrowserOpened.value = true
}

function hideComponentBrowser() {
  graphStore.editedNodeInfo = undefined
  componentBrowserOpened.value = false
  displayedDocs.value = undefined
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

function commitComponentBrowser(
  content: string,
  requiredImports: RequiredImport[],
  type: Typename | undefined,
) {
  if (graphStore.editedNodeInfo) {
    // We finish editing a node.
    graphStore.setNodeContent(graphStore.editedNodeInfo.id, content, requiredImports)
  } else if (content != '') {
    // We finish creating a new node.
    scheduleCreateNode({
      placement: { type: 'fixed', position: componentBrowserNodePosition.value },
      expression: content,
      type,
      requiredImports,
    })
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

const componentBrowser = ref<ComponentInstance<typeof ComponentBrowser>>()
const docPanel = ref<ComponentInstance<typeof RightDockPanel>>()

const componentBrowserElements = computed(() => [componentBrowser.value?.$el, docPanel.value?.$el])

// === Node Creation ===

interface NewNodeOptions {
  placement: PlacementStrategy
  sourcePort?: Ast.AstId | undefined
}

function addNodeDisconnected() {
  nodeSelection.deselectAll()
  createWithComponentBrowser({ placement: { type: 'viewport' } })
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
  if (sourcePort == null) return
  const sourcePortAst = graphStore.viewModule.get(sourcePort) as Ast.Expression
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

function handleNodeOutputPortDoubleClick(id: Ast.AstId) {
  const srcNode = graphStore.db.getPatternExpressionNodeId(id)
  if (srcNode == null) {
    console.error('Impossible happened: Double click on port not belonging to any node: ', id)
    return
  }
  createWithComponentBrowser({ placement: { type: 'source', node: srcNode }, sourcePort: id })
}

function handleEdgeDrop(source: Ast.AstId, position: Vec2) {
  createWithComponentBrowser({ placement: { type: 'fixed', position }, sourcePort: source })
}

// === Node Collapsing ===

function collapseNodes(nodes: Node[]) {
  const selected = new Set(
    iter.map(
      iter.filter(nodes, ({ type }) => type === 'component'),
      nodeId,
    ),
  )
  if (selected.size == 0) return
  try {
    const info = prepareCollapsedInfo(selected, graphStore.db)
    if (!info.ok) {
      toasts.userActionFailed.show(`Unable to group nodes: ${info.error.payload}.`)
      return
    }
    const currentMethodName = unwrapOr(graphStore.currentMethodPointer, undefined)?.name
    if (currentMethodName == null) {
      bail(`Cannot get the method name for the current execution stack item.`)
    }
    const topLevel = graphStore.moduleRoot
    if (!topLevel) {
      bail('BUG: no top level, collapsing not possible.')
    }
    const selectedNodeRects = iter.filterDefined(iter.map(selected, graphStore.visibleArea))
    graphStore.edit((edit) => {
      const { collapsedCallRoot, collapsedNodeIds, outputAstId } = performCollapse(
        info.value,
        edit.getVersion(topLevel),
        graphStore.db,
        currentMethodName,
      )
      const position = collapsedNodePlacement(selectedNodeRects)
      edit.get(collapsedCallRoot).mutableNodeMetadata().set('position', position.xy())
      if (outputAstId != null) {
        const collapsedNodeRects = iter.filterDefined(
          iter.map(collapsedNodeIds, graphStore.visibleArea),
        )
        const { place } = usePlacement(collapsedNodeRects, graphNavigator.viewport)
        const position = place(collapsedNodeRects)
        edit.get(outputAstId).mutableNodeMetadata().set('position', position.xy())
      }
    })
  } catch (err) {
    console.error('Error while collapsing, this is not normal.', err)
  }
}

// === Drag and drop ===

async function handleFileDrop(event: DragEvent) {
  // A vertical gap between created nodes when multiple files were dropped together.
  const MULTIPLE_FILES_GAP = 50

  if (!event.dataTransfer?.items) return
  ;[...event.dataTransfer.items].forEach(async (item, index) => {
    if (item.kind === 'file') {
      if (!graphStore.methodAst.ok) return
      const file = item.getAsFile()
      if (!file) return
      const clientPos = new Vec2(event.clientX, event.clientY)
      const offset = new Vec2(0, index * -MULTIPLE_FILES_GAP)
      const pos = graphNavigator.clientToScenePos(clientPos).add(offset)
      const uploader = Uploader.Create(
        projectStore,
        file,
        pos,
        projectStore.isOnLocalBackend,
        event.shiftKey,
        graphStore.methodAst.value.externalId,
      )
      const uploadResult = await uploader.upload()
      if (uploadResult.ok) {
        scheduleCreateNode({
          placement: { type: 'mouseEvent', position: pos },
          expression: uploadedExpression(uploadResult.value),
        })
      } else {
        uploadResult.error.log(`Uploading file failed`)
      }
    }
  })
}

// === Color Picker ===

provideNodeColors(graphStore, (variable) =>
  viewportNode.value ? getComputedStyle(viewportNode.value).getPropertyValue(variable) : '',
)

const groupColors = computed(() => {
  const styles: { [key: string]: string } = {}
  for (const group of suggestionDb.groups) {
    styles[groupColorVar(group)] = group.color ?? colorFromString(group.name)
  }
  return styles
})
</script>

<template>
  <div
    ref="rootNode"
    class="GraphEditor"
    :class="{ draggingEdge: graphStore.mouseEditedEdge != null }"
    :style="groupColors"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <div class="vertical">
      <div ref="viewportNode" class="viewport" @click="handleClick">
        <GraphMissingView v-if="graphMissing" />
        <template v-else>
          <GraphNodes
            @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick"
            @enterNode="(id) => stackNavigator.enterNode(id)"
            @createNodes="createNodesFromSource"
            @toggleDocPanel="toggleRightDockHelpPanel"
          />
          <GraphEdges :navigator="graphNavigator" @createNodeFromEdge="handleEdgeDrop" />
          <ComponentBrowser
            v-if="componentBrowserOpened"
            ref="componentBrowser"
            :navigator="graphNavigator"
            :nodePosition="componentBrowserNodePosition"
            :usage="componentBrowserUsage"
            :associatedElements="componentBrowserElements"
            @accepted="commitComponentBrowser"
            @canceled="hideComponentBrowser"
            @selectedSuggestionId="displayedDocs = $event"
            @isAiPrompt="aiMode = $event"
          />
          <PlusButton title="Add Component" @click.stop="addNodeDisconnected()" />
        </template>
        <TopBar
          v-model:recordMode="projectStore.recordMode"
          v-model:showCodeEditor="showCodeEditor"
          :showDocumentationEditor="rightDock.visible"
          :zoomLevel="100.0 * graphNavigator.targetScale"
          :class="{ extraRightSpace: !rightDock.visible }"
          @fitToAllClicked="zoomToSelected"
          @zoomIn="graphNavigator.stepZoom(+1)"
          @zoomOut="graphNavigator.stepZoom(-1)"
          @update:showDocumentationEditor="rightDock.setVisible"
        />
        <SceneScroller
          :navigator="graphNavigator"
          :scrollableArea="Rect.Bounding(...graphStore.visibleNodeAreas)"
        />
        <GraphMouse />
        <div
          ref="floatingLayer"
          class="floatingLayer"
          :style="{ transform: graphNavigator.transform }"
        />
      </div>
      <BottomPanel v-model:show="showCodeEditor">
        <Suspense>
          <CodeEditor ref="codeEditor" />
        </Suspense>
      </BottomPanel>
    </div>
    <RightDockPanel ref="docPanel" v-model:displayedDocs="displayedDocs" :aiMode="aiMode" />
  </div>
</template>

<style scoped>
.GraphEditor {
  width: 100%;
  height: 100%;
  contain: layout;
  user-select: none;
  /* Prevent touchpad back gesture, which can be triggered while panning. */
  overscroll-behavior-x: none;

  display: flex;
  flex-direction: row;
  & .DockPanel {
    flex: none;
  }
  & .vertical {
    flex: auto;
    min-width: 0;
  }
}

.vertical {
  display: flex;
  flex-direction: column;
  & .BottomPanel {
    flex: none;
  }
  & .viewport {
    flex: auto;
    min-height: 0;
  }
}

.viewport {
  position: relative; /* Needed for safari when using contain: layout */
  contain: layout;
  overflow: clip;
  touch-action: none;
  --node-color-no-type: #596b81;
  --output-node-color: #006b8a;
}

.floatingLayer {
  position: absolute;
  top: 0;
  left: 0;
  /* The size isn't important, except it must be non-zero for `floating-ui` to calculate the scale factor. */
  width: 1px;
  height: 1px;
  contain: layout size style;
  will-change: transform;
  pointer-events: none;
  > * {
    pointer-events: auto;
  }
}
</style>
