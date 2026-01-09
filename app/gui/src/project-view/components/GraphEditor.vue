<script setup lang="ts">
import {
  useCurrentProject,
  useGraphStore,
  useProjectNames,
  useProjectStore,
  useSuggestionDbStore,
  useWidgetRegistry,
} from '$/components/WithCurrentProject.vue'
import type { Node, NodeId } from '$/providers/openedProjects/graph'
import { isInputNode, nodeId } from '$/providers/openedProjects/graph/graphDatabase'
import type { RequiredImport } from '$/providers/openedProjects/module/imports'
import { provideNodeExecution } from '$/providers/openedProjects/project/nodeExecution'
import type { SuggestionId, Typename } from '$/providers/openedProjects/suggestionDatabase/entry'
import { suggestionDocumentationUrl } from '$/providers/openedProjects/suggestionDatabase/entry'
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
import { performCollapse, prepareCollapsedInfo } from '@/components/GraphEditor/collapsing'
import { useGraphEditorClipboard } from '@/components/GraphEditor/graphClipboard'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { selectionActionHandlers } from '@/components/GraphEditor/selectionActions'
import { createSelectionAlignmentHandlers } from '@/components/GraphEditor/selectionAlignment'
import { useGraphEditorToasts } from '@/components/GraphEditor/toasts'
import { uploadedExpression, Uploader } from '@/components/GraphEditor/upload'
import GraphMissingView from '@/components/GraphMissingView.vue'
import GraphMouse from '@/components/GraphMouse.vue'
import PopoverRootProvider from '@/components/PopoverRootProvider.vue'
import SceneScroller from '@/components/SceneScroller.vue'
import TopBar from '@/components/TopBar.vue'
import { builtinWidgets } from '@/components/widgets'
import { useDoubleClick } from '@/composables/doubleClick'
import { unrefElement, useEventConditional } from '@/composables/events'
import type { PlacementStrategy } from '@/composables/nodeCreation'
import { registerHandlers, toggledAction, type DisplayableActionName } from '@/providers/action'
import { useGlobalEventRegistry } from '@/providers/globalEventRegistry'
import { provideGraphEditorState } from '@/providers/graphEditorState'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { provideGraphNavigator } from '@/providers/graphNavigator'
import { provideNodeColors } from '@/providers/graphNodeColors'
import { provideNodeCreation } from '@/providers/graphNodeCreation'
import { provideGraphSelection } from '@/providers/graphSelection'
import { provideStackNavigator } from '@/providers/graphStackNavigator'
import { injectKeyboard } from '@/providers/keyboard'
import { provideLanguageSupportExtensions } from '@/providers/languageSupportExtensions'
import { providePersisted } from '@/stores/persisted'
import { provideVisualizationStore } from '@/stores/visualization'
import { assert, bail } from '@/util/assert'
import { Ast } from '@/util/ast'
import { partition } from '@/util/data/array'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { isDef, type VueInstance } from '@vueuse/core'
import * as iter from 'enso-common/src/utilities/data/iter'
import * as objects from 'enso-common/src/utilities/data/object'
import { Err, Ok, unwrapOr } from 'enso-common/src/utilities/data/result'
import { set } from 'lib0'
import {
  computed,
  onActivated,
  onDeactivated,
  onMounted,
  ref,
  toRaw,
  toRef,
  toValue,
  useTemplateRef,
  watch,
  watchEffect,
} from 'vue'
import { analyzeConnectAround } from './GraphEditor/detaching'
import { provideRenameSchedule } from './GraphEditor/widgets/WidgetFunctionName.vue'

const keyboard = injectKeyboard()
const rightPanel = useRightPanelData()
const projectStore = useProjectStore()
const projectNames = useProjectNames()
const graphStore = useGraphStore()
const { id: assetId, module, ensoPath } = useCurrentProject()
const widgetRegistry = useWidgetRegistry()
const suggestionDb = useSuggestionDbStore()
provideVisualizationStore(projectStore)
provideLanguageSupportExtensions({
  project: projectStore,
  projectNames,
  suggestionDb: suggestionDb.entries,
})

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
const graphMissing = computed(() => module.value.root != null && !graphStore.currentMethod.ast.ok)

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

const detachInfo = computed(() => analyzeConnectAround(nodeSelection.selected, graphStore))

// === Node creation ===

const { place: nodePlacement, collapse: collapsedNodePlacement } = usePlacement(
  toRef(graphStore, 'visibleNodeAreas'),
  toRef(graphNavigator, 'viewport'),
)

const { scheduleCreateNode, createNodes, placeNode } = provideNodeCreation(
  module,
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
    enabled: () => graphNavigator.sceneMousePos != null && !componentBrowserOpened.value,
    action: () => createWithComponentBrowser(fromSelection() ?? { placement: { type: 'mouse' } }),
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
      module.value.batchEdits(() => {
        for (const nodeId of selected) {
          graphStore.setNodeVisualization(nodeId, { visible: !allVisible })
        }
      })
    },
  },
  'graph.pasteNode': { action: createNodesFromClipboard },
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
  'graph.deleteSelectedEdge': {
    enabled: () =>
      nodeSelection.selectedEdge != null &&
      graphStore.db.connectionExists(nodeSelection.selectedEdge),
    action: () => {
      if (!nodeSelection.selectedEdge) return
      graphStore.updatePortValue(nodeSelection.selectedEdge.target, undefined)
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
    () => detachInfo.value.ok && detachInfo.value.value.length > 0,
    {
      collapseNodes,
      copyNodesToClipboard,
      ...createSelectionAlignmentHandlers(graphStore, module),
      deleteNodes: (nodes) => graphStore.deleteNodes(nodes.map(nodeId)),
      deleteAndConnectAround: (nodes) => {
        return module.value.edit(async (edit) => {
          if (!detachInfo.value.ok) return detachInfo.value
          const reconnectResults = await Promise.all(
            detachInfo.value.value.map(async ({ port, ident }) => {
              const result = await graphStore.updatePortValue(
                port,
                Ast.Ident.new(edit, ident),
                edit,
              )
              if (!result.ok) {
                result.error.log('Failed to connect around')
              }
              return result
            }),
          )
          if (reconnectResults.some((result) => !result.ok)) {
            toasts.userActionFailed.show(
              'Errors occurred while connecting around removed components.',
            )
          }
          for (const node of nodes) {
            // We cannot call graphStore.deleteNodes, because it bases on the graphDb
            // which is not updated with reconnections above.
            const outerAst = edit.getVersion(node.outerAst)
            if (outerAst.isStatement()) Ast.deleteFromParentBlock(outerAst)
          }
          return Ok()
        })
      },
    },
  ),
})

const isActive = ref(true)
onActivated(() => (isActive.value = true))
onDeactivated(() => (isActive.value = false))

const { globalEventRegistry } = useGlobalEventRegistry()
useEventConditional(globalEventRegistry, 'keydown', isActive, (e) => {
  return graphBindingsHandler(e) || graphNavigator.keyboardEvents.keydown(e)
})

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
    nodeSelection.selectedEdge = undefined
  },
  (e: MouseEvent) => {
    if (e.target !== e.currentTarget) return false
    stackNavigator.exitNode()
  },
)

// === Keyboard/Mouse bindings ===

const graphBindingsHandler = graphBindings.handler(
  objects.mapEntries(graphBindings.bindings, (actionName) => {
    const actionDef = actionHandlers[actionName]
    return () => {
      if (toValue(actionDef.enabled) === false) return false
      void actionDef.action()
    }
  }),
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
  rightPanel.setContext(ensoPath.value, {
    item: assetId.value,
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

const root = useTemplateRef<HTMLElement>('root')

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
  const sourcePortAst = module.value.ast?.get(sourcePort)
  assert(sourcePortAst?.isExpression() === true)
  const [toCommit, toEdit] = partition(options, (opts) => opts.commit)
  createNodes(
    toCommit.map((options: NodeCreationOptions) => ({
      placement:
        options.position ?
          { type: 'fixed', position: options.position }
        : { type: 'source', node: sourceNode },
      expression: options.content!.instantiateCopied([sourcePortAst]).code(),
    })),
  )
  if (toEdit.length) {
    const placement: PlacementStrategy =
      toEdit[0]?.position ?
        { type: 'fixed', position: toEdit[0].position }
      : { type: 'source', node: sourceNode }
    createWithComponentBrowser({ placement, sourcePort })
  }
}

function createNodeFromPort(id: Ast.AstId) {
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
const renameSchedule = provideRenameSchedule()

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
    const topLevel = module.value.root
    if (!topLevel) {
      bail('BUG: no top level, creating User Defined Component not possible.')
    }
    const selectedNodeRects = iter.filterDefined(iter.map(selected, graphStore.visibleArea))
    module.value.edit((edit) => {
      const { collapsedCallRoot, collapsedNodeIds, outputAstId, collapsedName } = performCollapse(
        info.value,
        edit.getVersion(topLevel),
        graphStore.db,
        currentMethodName,
      )
      const position = collapsedNodePlacement(selectedNodeRects)
      edit.get(collapsedCallRoot).mutableNodeMetadata().set('position', position.xy())

      const collapsedNodeRects = iter.filterDefined(
        iter.map(collapsedNodeIds, graphStore.visibleArea),
      )
      const { place } = usePlacement(collapsedNodeRects, graphNavigator.viewport)
      const outputPosition = place(collapsedNodeRects)
      edit.get(outputAstId).mutableNodeMetadata().set('position', outputPosition.xy())

      if (graphStore.currentMethod.pointer.ok) {
        const currentPointer = graphStore.currentMethod.pointer.value
        renameSchedule?.scheduleFunctionRename({ ...currentPointer, name: collapsedName })
      }

      return Ok()
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
  'graph.pasteNode',
  'graph.toggleCodeEditor',
  'graph.toggleDocumentationEditor',
]
</script>

<template>
  <div
    ref="root"
    class="GraphEditor vertical"
    :class="{ draggingEdge: graphStore.mouseEditedEdge != null }"
    @dragover.prevent
    @drop.prevent="handleFileDrop($event)"
  >
    <PopoverRootProvider class="viewportPanel">
      <ContextMenuTrigger
        ref="viewportNode"
        class="viewport"
        :actions="contextMenuActions"
        @click="handleClick"
      >
        <GraphMissingView v-if="graphMissing" />
        <template v-else>
          <GraphNodes
            @enterNode="(id) => stackNavigator.enterNode(id)"
            @createNodes="createNodesFromSource"
            @toggleDocPanel="toggleRightDockHelpPanel"
            @contextmenu.stop.prevent
          />
          <GraphEdges
            :navigator="graphNavigator"
            @createNodeFromEdge="handleEdgeDrop"
            @createNodeFromPort="createNodeFromPort"
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
    </PopoverRootProvider>
    <PopoverRootProvider>
      <BottomPanel v-model:show="showCodeEditor" class="bottomPanel">
        <CodeEditor />
      </BottomPanel>
    </PopoverRootProvider>
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
}

.vertical {
  display: flex;
  flex-direction: column;
  & :deep(.bottomPanel) {
    flex: none;
  }
  & .viewportPanel {
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
  width: 100%;
  height: 100%;
  --node-color-no-type: #596b81;
  --output-node-color: #006b8a;
}
</style>
