<script setup lang="ts">
import { codeEditorBindings, graphBindings, interactionBindings } from '@/bindings'
import CodeEditor from '@/components/CodeEditor.vue'
import ColorPicker from '@/components/ColorPicker.vue'
import ComponentBrowser from '@/components/ComponentBrowser.vue'
import { type Usage } from '@/components/ComponentBrowser/input'
import {
  DEFAULT_NODE_SIZE,
  mouseDictatedPlacement,
  usePlacement,
} from '@/components/ComponentBrowser/placement'
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
import { assertNever, bail } from '@/util/assert'
import type { AstId } from '@/util/ast/abstract'
import type { Pattern } from '@/util/ast/match'
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

function zoomToSelected() {
  if (!viewportNode.value) return

  const allNodes = graphStore.db.nodeIdToNode
  const validSelected = [...nodeSelection.selected].filter((id) => allNodes.has(id))
  const nodesToCenter = validSelected.length === 0 ? allNodes.keys() : validSelected
  let bounds = Rect.Bounding()
  for (const id of nodesToCenter) {
    const rect = graphStore.visibleArea(id)
    if (rect) bounds = Rect.Bounding(bounds, rect)
  }
  if (bounds.isFinite())
    graphNavigator.panAndZoomTo(bounds, 0.1, Math.max(1, graphNavigator.targetScale))
}

// == Breadcrumbs ==

const stackNavigator = useStackNavigator()

// === Toasts ===

useGraphEditorToasts()

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

// === Clipboard Copy/Paste ===

const { copySelectionToClipboard, createNodesFromClipboard } = useGraphEditorClipboard(
  nodeSelection,
  graphNavigator,
)

// === Interactions ===

const interaction = provideInteractionHandler()
const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.handleCancel(),
})

const { place: nodePlacement, collapse: collapsedNodePlacement } = usePlacement(
  toRef(graphStore, 'visibleNodeAreas'),
  toRef(graphNavigator, 'viewport'),
)

useEvent(window, 'keydown', (event) => {
  interactionBindingsHandler(event) ||
    (!keyboardBusy() && graphBindingsHandler(event)) ||
    (!keyboardBusyExceptIn(codeEditorArea.value) && codeEditorHandler(event))
})
useEvent(window, 'pointerdown', (e) => interaction.handlePointerDown(e, graphNavigator), {
  capture: true,
})

// === Keyboard/Mouse bindings ===

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
      createWithComponentBrowser(
        fromSelection() ?? {
          placement: [
            'fixed',
            mouseDictatedPlacement(DEFAULT_NODE_SIZE, graphNavigator.sceneMousePos).position,
          ],
        },
      )
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
    copySelectionToClipboard()
  },
  pasteNode() {
    if (keyboardBusy()) return false
    createNodesFromClipboard()
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
      const topLevel = graphStore.topLevel
      if (!topLevel) {
        bail('BUG: no top level, collapsing not possible.')
      }
      const selectedNodeRects = filterDefined(Array.from(selected, graphStore.visibleArea))
      graphStore.edit((edit) => {
        const { refactoredNodeId, collapsedNodeIds, outputNodeId } = performCollapse(
          info,
          edit.getVersion(topLevel),
          graphStore.db,
          currentMethodName,
        )
        const { position } = collapsedNodePlacement(selectedNodeRects)
        edit.get(refactoredNodeId).mutableNodeMetadata().set('position', position.xy())
        if (outputNodeId != null) {
          const collapsedNodeRects = filterDefined(
            Array.from(collapsedNodeIds, graphStore.visibleArea),
          )
          const { place } = usePlacement(collapsedNodeRects, graphNavigator.viewport)
          const { position } = place(collapsedNodeRects)
          edit.get(outputNodeId).mutableNodeMetadata().set('position', position.xy())
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
  changeColorSelectedNodes() {
    toggleColorPicker()
  },
})

const { handleClick } = useDoubleClick(
  (e: MouseEvent) => {
    graphBindingsHandler(e)
    if (document.activeElement instanceof HTMLElement) {
      document.activeElement.blur()
    }
    showColorPicker.value = false
  },
  () => {
    stackNavigator.exitNode()
  },
)

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
    placeNode(options.placement),
  )
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
  placement: PlacementType
  sourcePort?: AstId | undefined
}
type PlacementType = 'viewport' | ['source', NodeId] | ['fixed', Vec2]

const placeNode = (placement: PlacementType): Vec2 =>
  placement === 'viewport' ? nodePlacement().position
  : placement[0] === 'source' ?
    nodePlacement(filterDefined([graphStore.visibleArea(placement[1])])).position
  : placement[0] === 'fixed' ? placement[1]
  : assertNever(placement)

/**
 * Start creating a node, basing its inputs and position on the current selection, if any;
 * or the current viewport, otherwise.
 */
function addNodeAuto() {
  createWithComponentBrowser(fromSelection() ?? { placement: 'viewport' })
}

function fromSelection(): NewNodeOptions | undefined {
  if (graphStore.editedNodeInfo != null) return undefined
  const firstSelectedNode = set.first(nodeSelection.selected)
  return {
    placement: ['source', firstSelectedNode],
    sourcePort: graphStore.db.getNodeFirstOutputPort(firstSelectedNode),
  }
}

function createNode(placement: PlacementType, sourcePort: AstId, pattern: Pattern) {
  const position = placeNode(placement)
  const content = pattern.instantiateCopied([graphStore.viewModule.get(sourcePort)]).code()
  return graphStore.createNode(position, content, undefined, []) ?? undefined
}

function createNodesFromSource(sourceNode: NodeId, options: NodeCreationOptions[]) {
  const sourcePort = graphStore.db.getNodeFirstOutputPort(sourceNode)
  const [toCommit, toEdit] = partition(options, (opts) => opts.commit)
  const [withPos, withoutPos] = partition(toCommit, (opts) => !!opts.position)
  if (
    document.activeElement instanceof HTMLElement ||
    document.activeElement instanceof SVGElement
  ) {
    document.activeElement.blur()
  }
  const placementForOptions = (options: NodeCreationOptions): PlacementType =>
    options.position ? ['fixed', options.position] : ['source', sourceNode]
  const createWithoutEditing = (options: NodeCreationOptions) =>
    createNode(placementForOptions(options), sourcePort, options.content!)
  const created = new Set<NodeId>(
    filterDefined([...withPos.map(createWithoutEditing), ...withoutPos.map(createWithoutEditing)]),
  )
  if (created.size) nodeSelection.setSelection(created)
  for (const options of toEdit)
    createWithComponentBrowser({ placement: placementForOptions(options), sourcePort })
}

function handleNodeOutputPortDoubleClick(id: AstId) {
  const srcNode = graphStore.db.getPatternExpressionNodeId(id)
  if (srcNode == null) {
    console.error('Impossible happened: Double click on port not belonging to any node: ', id)
    return
  }
  createWithComponentBrowser({ placement: ['source', srcNode], sourcePort: id })
}

function handleEdgeDrop(source: AstId, position: Vec2) {
  createWithComponentBrowser({ placement: ['fixed', position], sourcePort: source })
}

// === Drag and drop ===

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

// === Color Picker ===

/** A small offset to keep the color picker slightly away from the nodes. */
const COLOR_PICKER_X_OFFSET_PX = -300
const showColorPicker = ref(false)
const colorPickerSelectedColor = ref('')

function overrideSelectedNodesColor(color: string) {
  ;[...nodeSelection.selected].map((id) => graphStore.overrideNodeColor(id, color))
}

/** Toggle displaying of the color picker. It will change colors of selected nodes. */
function toggleColorPicker() {
  if (nodeSelection.selected.size === 0) {
    showColorPicker.value = false
    return
  }
  showColorPicker.value = !showColorPicker.value
  if (showColorPicker.value) {
    const oneOfSelected = set.first(nodeSelection.selected)
    const color = graphStore.db.getNodeColorStyle(oneOfSelected)
    if (color.startsWith('var') && viewportNode.value != null) {
      // Some colors are defined in CSS variables, we need to get the actual color.
      const variableName = color.slice(4, -1)
      colorPickerSelectedColor.value = getComputedStyle(viewportNode.value).getPropertyValue(
        variableName,
      )
    } else {
      colorPickerSelectedColor.value = color
    }
  }
}
const colorPickerPos = computed(() => {
  const nodeRects = [...nodeSelection.selected].map(
    (id) => graphStore.nodeRects.get(id) ?? Rect.Zero,
  )
  const boundingRect = Rect.Bounding(...nodeRects)
  return new Vec2(boundingRect.left + COLOR_PICKER_X_OFFSET_PX, boundingRect.center().y)
})
const colorPickerStyle = computed(() =>
  colorPickerPos.value != null ?
    { transform: `translate(${colorPickerPos.value.x}px, ${colorPickerPos.value.y}px)` }
  : {},
)

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
        @toggleColorPicker="toggleColorPicker"
      />

      <ColorPicker
        class="colorPicker"
        :style="colorPickerStyle"
        :show="showColorPicker"
        :color="colorPickerSelectedColor"
        @update:color="overrideSelectedNodesColor"
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
      :breadcrumbs="stackNavigator.breadcrumbLabels.value"
      :allowNavigationLeft="stackNavigator.allowNavigationLeft.value"
      :allowNavigationRight="stackNavigator.allowNavigationRight.value"
      :zoomLevel="100.0 * graphNavigator.targetScale"
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

.colorPicker {
  position: absolute;
}
</style>
