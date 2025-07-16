<script setup lang="ts">
import {
  useGraphStore,
  useProjectNames,
  useProjectStore,
  useSuggestionDbStore,
  useWidgetRegistry,
} from '$/components/WithCurrentProject.vue'
import { useRightPanelData } from '$/providers/rightPanel'
import { graphBindings } from '@/bindings'
import BottomPanel from '@/components/BottomPanel.vue'
import CodeEditor from '@/components/CodeEditor.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import type { Usage } from '@/components/ComponentBrowser/input'
import { usePlacement } from '@/components/ComponentBrowser/placement'
import ContextMenuTrigger from '@/components/ContextMenuTrigger.vue'
import GraphEdges from '@/components/GraphEditor/GraphEdges.vue'
import GraphNodes from '@/components/GraphEditor/GraphNodes.vue'
import { useGraphEditorClipboard } from '@/components/GraphEditor/clipboard'
import { performCollapse, prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { selectionActionHandlers } from '@/components/GraphEditor/selectionActions'
import { useGraphEditorToasts } from '@/components/GraphEditor/toasts'
import { uploadedExpression, Uploader } from '@/components/GraphEditor/upload'
import GraphMissingView from '@/components/GraphMissingView.vue'
import GraphMouse from '@/components/GraphMouse.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import TopBar from '@/components/TopBar.vue'
import { builtinWidgets } from '@/components/widgets'
import { useDoubleClick } from '@/composables/doubleClick'
import { unrefElement, useEvent } from '@/composables/events'
import type { PlacementStrategy } from '@/composables/nodeCreation'
import { type DisplayableActionName, registerHandlers, toggledAction } from '@/providers/action'
import { provideGraphEditorState } from '@/providers/graphEditorState'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideNodeColors } from '@/providers/graphNodeColors'
import { provideNodeCreation } from '@/providers/graphNodeCreation'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideStackNavigator } from '@/providers/graphStackNavigator'
import { injectKeyboard } from '@/providers/keyboard'
import { providePopoverRoot } from '@/providers/popoverRoot'
import type { Node, NodeId } from '@/stores/graph'
import { isInputNode, nodeId } from '@/stores/graph/graphDatabase'
import type { RequiredImport } from '@/stores/graph/imports'
import { providePersisted } from '@/stores/persisted'
import { provideNodeExecution } from '@/stores/project/nodeExecution'
import type { SuggestionId, Typename } from '@/stores/suggestionDatabase/entry'
import { suggestionDocumentationUrl } from '@/stores/suggestionDatabase/entry'
import { provideVisualizationStore } from '@/stores/visualization'
import { assert, bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import { partition } from '@/util/data/array'
import { Rect } from '@/util/data/rect'
import { Err, Ok, unwrapOr } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { isDef, VueInstance } from '@vueuse/core'
import * as iter from 'enso-common/src/utilities/data/iter'
import * as objects from 'enso-common/src/utilities/data/object'
import { set } from 'lib0'
import { computed, onMounted, ref, toRaw, toRef, useTemplateRef, watch, watchEffect } from 'vue'

const keyboard = injectKeyboard()
const rightPanel = useRightPanelData()
const projectStore = useProjectStore()
const projectNames = useProjectNames()
const graphStore = useGraphStore()
const widgetRegistry = useWidgetRegistry()
const suggestionDb = useSuggestionDbStore()
const _visualizationStore = provideVisualizationStore(projectStore)

const nodeExecution = provideNodeExecution(projectStore)
;(window as any)._mockSuggestion = suggestionDb.mockSuggestion

onMounted(() => {
  widgetRegistry.loadWidgets(Object.entries(builtinWidgets))
  if (import.meta.env.DEV) {
    ;(window as any).suggestionDb = toRaw(suggestionDb.entries)
  }
})

// === Navigator ===

const viewportNode = useTemplateRef<VueInstance>('viewportNode')
const viewportElem = computed(() => unrefElement<HTMLElement>(viewportNode))
onMounted(() => viewportElem.value?.focus())
const graphNavigator: GraphNavigator = provideGraphNavigator(viewportNode, keyboard, {
  predicate: (e) => (e instanceof KeyboardEvent ? nodeSelection.selected.size === 0 : true),
})

providePopoverRoot(viewportElem)

// === Client saved state ===

providePersisted(
  () => projectStore.id,
  graphStore,
  graphNavigator,
  () => zoomToAll(true),
)

// === Zoom/pan ===

const scrollBounds = computed(() => Rect.Bounding(...graphStore.visibleNodeAreas) ?? Rect.Zero)

function nodesBounds(nodeIds: Iterable<NodeId>) {
  return Rect.Bounding(...Array.from(nodeIds, (id) => graphStore.visibleArea(id)).filter(isDef))
}

function selectionBounds() {
  return nodesBounds(nodeSelection.selected) ?? scrollBounds.value
}

function zoomToSelected() {
  const bounds = selectionBounds()
  if (bounds) graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale))
}

function zoomToAll(skipAnimation: boolean = false) {
  const bounds = scrollBounds.value
  if (bounds)
    graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale), skipAnimation)
}

function panToSelected() {
  const bounds = selectionBounds()
  if (bounds)
    graphNavigator.panTo([new Vec2(bounds.left, bounds.top), new Vec2(bounds.right, bounds.bottom)])
}

// == Breadcrumbs ==

const projectNameEdited = ref(false)
const stackNavigator = provideStackNavigator(projectStore, graphStore, projectNames)
const graphMissing = computed(
  () => graphStore.moduleRoot != null && !graphStore.currentMethod.ast.ok,
)

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
    onSoleSelected: (id) => graphStore.db.moveNodeToTop(id),
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

// === Action Handlers ===

const showCodeEditor = ref(false)

const actionHandlers = registerHandlers({
  'graphEditor.showHelp': {
    action: () => rightPanel.toggleTab('help'),
    toggled: computed(() => rightPanel.tab === 'help'),
  },
  'graph.renameProject': toggledAction(projectNameEdited),
  'graph.addComponent': {
    action: (ctx) => {
      nodeSelection.deselectAll()
      const clientPos = ctx?.openPosition
      const placement: PlacementStrategy =
        clientPos ?
          { type: 'fixed', position: graphNavigator.clientToScenePos(Vec2.FromXY(clientPos)) }
        : { type: 'viewport' }
      createWithComponentBrowser({ placement })
    },
  },
  'graph.toggleCodeEditor': toggledAction(showCodeEditor),
  'graph.toggleDocumentationEditor': {
    action: () => rightPanel.toggleTab('documentation'),
    toggled: () => rightPanel.tab === 'documentation',
  },
  'graph.refreshExecution': {
    action: () => nodeExecution.recomputeAll(),
  },
  'graph.recomputeAll': {
    action: () => nodeExecution.recomputeAll('Live'),
  },
  'graph.undo': {
    enabled: () => graphStore.undoManager.canUndo,
    action: () => graphStore.undoManager.undo(),
  },
  'graph.redo': {
    enabled: () => graphStore.undoManager.canRedo,
    action: () => graphStore.undoManager.redo(),
  },
  'graph.fitAll': {
    action: zoomToSelected,
  },
  'graph.zoomIn': {
    action: () => graphNavigator.stepZoom(+1),
  },
  'graph.zoomOut': {
    action: () => graphNavigator.stepZoom(-1),
  },
  'graph.navigateUp': {
    available: stackNavigator.hasBreadcrumbsBeyondRoot,
    enabled: stackNavigator.allowNavigationLeft,
    action: () => stackNavigator.exitNode(),
  },
  'component.enterNode': {
    // TODO: Unify with handler in GraphNode.
    action: () => {
      const selectedNode = set.first(nodeSelection.selected)
      if (selectedNode) {
        stackNavigator.enterNode(selectedNode)
      }
    },
  },
  'graph.startProfiling': { action: () => void projectStore.lsRpcConnection.profilingStart(true) },
  'graph.stopProfiling': { action: () => void projectStore.lsRpcConnection.profilingStop() },
  'graph.openComponentBrowser': {
    action: () => {
      if (graphNavigator.sceneMousePos != null && !componentBrowserOpened.value) {
        createWithComponentBrowser(fromSelection() ?? { placement: { type: 'mouse' } })
      }
    },
  },
  'graph.selectAll': { action: () => nodeSelection.selectAll() },
  'graph.deselectAll': {
    action: () => {
      nodeSelection.deselectAll()
      clearFocus()
      graphStore.undoManager.undoStackBoundary()
    },
  },
  'graph.toggleVisualization': {
    action: () => {
      // TODO: Merge with component action
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
  },
  'graph.pasteNode': { action: () => createNodesFromClipboard() },
  'graph.openDocumentation': {
    action: () => {
      const result = tryGetSelectionDocUrl()
      if (!result.ok) {
        toasts.userActionFailed.show(result.error.message('Unable to show node documentation'))
        return
      }
      window.open(result.value, '_blank')
    },
  },
  ...selectionActionHandlers(
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
  ),
})

useEvent(
  window,
  'keydown',
  (e) => graphBindingsHandler(e) || graphNavigator.keyboardEvents.keydown(e),
)

function tryGetSelectionDocUrl() {
  const selected = nodeSelection.tryGetSingleSelectedNode()
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

// === Keyboard/Mouse bindings ===

const graphBindingsHandler = graphBindings.handler(
  objects.mapEntries(
    graphBindings.bindings,
    (actionName) => () => void actionHandlers[actionName].action(),
  ),
)

// === Documentation Editor ===

const overrideDisplayedDocs = ref<SuggestionId>()
const aiMode = ref<boolean>(false)
const docsForSelection = computed(() => {
  const selected = nodeSelection.tryGetSingleSelectedNode()
  if (!selected.ok) return Err('Select a single component to display help')
  const suggestionId = graphStore.db.nodeMainSuggestionId.lookup(selected.value)
  if (suggestionId == null) return Err('No documentation available for selected component')
  return Ok(suggestionId)
})
const displayedDocs = computed(() =>
  overrideDisplayedDocs.value ? Ok(overrideDisplayedDocs.value) : docsForSelection.value,
)

watchEffect(() => {
  const projectId = projectStore.id
  rightPanel.setContext(projectId, {
    item: projectId,
    help: { item: displayedDocs.value, aiMode: aiMode.value },
  })
})

function toggleRightDockHelpPanel() {
  rightPanel.setTab('help')
}

// === Component Browser ===

const { componentBrowserOpened } = provideGraphEditorState({
  componentBrowserOpened: ref(false),
})
const componentBrowserNodePosition = ref<Vec2>(Vec2.Zero)
const componentBrowserUsage = ref<Usage>({ type: 'newNode' })

function openComponentBrowser(usage: Usage, position: Vec2) {
  componentBrowserUsage.value = usage
  componentBrowserNodePosition.value = position
  componentBrowserOpened.value = true
}

function hideComponentBrowser() {
  graphStore.editedNodeInfo = undefined
  componentBrowserOpened.value = false
  overrideDisplayedDocs.value = undefined
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

const root = ref<HTMLElement>()

// === Node Creation ===

interface NewNodeOptions {
  placement: PlacementStrategy
  sourcePort?: Ast.AstId | undefined
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
  const sourcePortAst = graphStore.viewModule.get(sourcePort)
  assert(sourcePortAst.isExpression())
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
      toasts.userActionFailed.show(
        `Unable to create User Defined Component: ${info.error.payload}.`,
      )
      return
    }
    const currentMethodName = unwrapOr(graphStore.currentMethod.pointer, undefined)?.name
    if (currentMethodName == null) {
      bail(`Cannot get the method name for the current execution stack item.`)
    }
    const topLevel = graphStore.moduleRoot
    if (!topLevel) {
      bail('BUG: no top level, creating User Defined Component not possible.')
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
    console.error('Error while creating User Defined Component, this is not normal.', err)
  }
}

// === Drag and drop ===

async function handleFileDrop(event: DragEvent) {
  // A vertical gap between created nodes when multiple files were dropped together.
  const MULTIPLE_FILES_GAP = 50

  if (!event.dataTransfer?.items) return
  ;[...event.dataTransfer.items].forEach(async (item, index) => {
    if (item.kind === 'file') {
      if (!graphStore.currentMethod.ast.ok) return
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
        graphStore.currentMethod.ast.value.externalId,
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
  viewportElem.value ? getComputedStyle(viewportElem.value).getPropertyValue(variable) : '',
)

const contextMenuActions: DisplayableActionName[] = [
  'graph.navigateUp',
  'graph.renameProject',
  'graph.refreshExecution',
  'graph.recomputeAll',
  'graph.undo',
  'graph.redo',
  'graph.addComponent',
  'graph.fitAll',
  'graph.toggleCodeEditor',
  'graph.toggleDocumentationEditor',
]
</script>

<template>
  <div
    class="GraphEditor"
    :class="{ draggingEdge: graphStore.mouseEditedEdge != null }"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <div class="vertical">
      <ContextMenuTrigger
        ref="viewportNode"
        class="viewport"
        :actions="contextMenuActions"
        @click="handleClick"
      >
        <GraphMissingView v-if="graphMissing" />
        <template v-else>
          <GraphNodes
            @nodeOutputPortDoubleClick="handleNodeOutputPortDoubleClick"
            @enterNode="(id) => stackNavigator.enterNode(id)"
            @createNodes="createNodesFromSource"
            @toggleDocPanel="toggleRightDockHelpPanel"
            @contextmenu.stop.prevent
          />
          <GraphEdges
            :navigator="graphNavigator"
            @createNodeFromEdge="handleEdgeDrop"
            @createNodeFromPort="createNodesFromSource"
            @outputPortDoubleClick="handleNodeOutputPortDoubleClick"
          />
          <ComponentBrowser
            v-if="componentBrowserOpened"
            ref="componentBrowser"
            :navigator="graphNavigator"
            :nodePosition="componentBrowserNodePosition"
            :usage="componentBrowserUsage"
            :graphEditorRoot="root"
            @accepted="commitComponentBrowser"
            @canceled="hideComponentBrowser"
            @selectedSuggestionId="overrideDisplayedDocs = $event"
            @isAiPrompt="aiMode = $event"
          />
        </template>
        <TopBar
          v-model:projectNameEdited="projectNameEdited"
          :zoomLevel="100.0 * graphNavigator.targetScale"
          :menuActions="contextMenuActions"
          @contextmenu.stop.prevent
        />
        <SceneScroller :navigator="graphNavigator" :scrollableArea="scrollBounds" />
        <GraphMouse />
      </ContextMenuTrigger>
      <BottomPanel v-model:show="showCodeEditor">
        <CodeEditor />
      </BottomPanel>
    </div>
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
    overflow-x: hidden;
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

.viewport.viewport {
  position: relative; /* Needed for safari when using contain: layout */
  display: block;
  contain: layout;
  overflow: clip;
  touch-action: none;
  --node-color-no-type: #596b81;
  --output-node-color: #006b8a;
}
</style>
