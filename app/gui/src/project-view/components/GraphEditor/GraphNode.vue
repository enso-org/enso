<script setup lang="ts">
import { graphBindings, nodeEditBindings } from '@/bindings'
import ComponentContextMenu from '@/components/ComponentContextMenu.vue'
import ComponentMenu from '@/components/ComponentMenu.vue'
import ComponentWidgetTree, {
  GRAB_HANDLE_X_MARGIN_L,
  GRAB_HANDLE_X_MARGIN_R,
  ICON_WIDTH,
} from '@/components/GraphEditor/ComponentWidgetTree.vue'
import GraphNodeComment from '@/components/GraphEditor/GraphNodeComment.vue'
import GraphNodeMessage, {
  colorForMessageType,
  iconForMessageType,
  type MessageType,
} from '@/components/GraphEditor/GraphNodeMessage.vue'
import GraphNodeOutputPorts from '@/components/GraphEditor/GraphNodeOutputPorts.vue'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import PointFloatingMenu from '@/components/PointFloatingMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useDoubleClick } from '@/composables/doubleClick'
import { usePointer, useResizeObserver } from '@/composables/events'
import { provideComponentButtons } from '@/providers/componentButtons'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectKeyboard } from '@/providers/keyboard'
import { useGraphStore, type Node } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import { useNodeExecution } from '@/stores/project/nodeExecution'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { prefixes } from '@/util/ast/node'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { computed, onUnmounted, ref, shallowRef, watch, watchEffect } from 'vue'
import type { ExternalId, VisualizationIdentifier } from 'ydoc-shared/yjsModel'

const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50
const CONTENT_PADDING = 4
const CONTENT_PADDING_PX = `${CONTENT_PADDING}px`
const MENU_CLOSE_TIMEOUT_MS = 300

const contentNodeStyle = {
  padding: CONTENT_PADDING_PX,
}

const props = defineProps<{
  node: Node
  edited: boolean
}>()

const emit = defineEmits<{
  dragging: [offset: Vec2]
  draggingCommited: []
  draggingCancelled: []
  replaceSelection: []
  outputPortClick: [event: PointerEvent, portId: AstId]
  outputPortDoubleClick: [event: PointerEvent, portId: AstId]
  enterNode: []
  createNodes: [options: NodeCreationOptions[]]
  setNodeColor: [color: string | undefined]
  toggleDocPanel: []
  'update:edited': [cursorPosition: number]
  'update:rect': [rect: Rect]
  'update:hoverAnim': [progress: number]
  'update:visualizationId': [id: Opt<VisualizationIdentifier>]
  'update:visualizationRect': [rect: Rect | undefined]
  'update:visualizationEnabled': [enabled: boolean]
  'update:visualizationWidth': [width: number]
  'update:visualizationHeight': [height: number]
}>()

const nodeSelection = injectGraphSelection(true)
const projectStore = useProjectStore()
const graph = useGraphStore()
const navigator = injectGraphNavigator(true)
const nodeExecution = useNodeExecution()

const nodeId = computed(() => asNodeId(props.node.rootExpr.externalId))
const potentialSelfArgumentId = computed(() => props.node.primarySubject)

const nodePosition = computed(() => {
  // Positions of nodes that are not yet placed are set to `Infinity`.
  if (props.node.position.equals(Vec2.Infinity)) return Vec2.Zero
  return props.node.position
})

onUnmounted(() => graph.unregisterNodeRect(nodeId.value))

const rootNode = ref<HTMLElement>()
const contentNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)

function inputExternalIds() {
  const externalIds = new Array<ExternalId>()
  for (const inputId of graph.db.nodeDependents.reverseLookup(nodeId.value)) {
    if (inputId) {
      externalIds.push(inputId)
    }
  }
  return externalIds
}

function getPanic(id: ExternalId) {
  const info = projectStore.computedValueRegistry.db.get(id)
  return info?.payload.type === 'Panic' ? info.payload.message : undefined
}

function getDataflowError(id: ExternalId) {
  return projectStore.dataflowErrors.lookup(id)?.value?.message
}

interface Message {
  type: MessageType
  text: string
  alwaysShow: boolean
}
const availableMessage = computed<Message | undefined>(() => {
  const externalId = nodeId.value
  if (!externalId) return undefined
  const info = projectStore.computedValueRegistry.db.get(externalId)
  switch (info?.payload.type) {
    case 'Panic': {
      const text = info.payload.message
      const alwaysShow = !inputExternalIds().some((id) => getPanic(id) === text)
      return { type: 'panic', text, alwaysShow } satisfies Message
    }
    case 'DataflowError': {
      const rawText = getDataflowError(externalId)
      const text = rawText?.split(' (at')[0]
      if (!text) return undefined
      const alwaysShow = !inputExternalIds().some((id) => getDataflowError(id) === rawText)
      const type = rawText.includes('Missing_Argument') ? 'missing' : 'error'
      return { type, text, alwaysShow } satisfies Message
    }
    case 'Value': {
      const warning = info.payload.warnings?.value
      if (!warning) return undefined
      return {
        type: 'warning',
        text: 'Warning: ' + warning,
        alwaysShow: false,
      } satisfies Message
    }
    default:
      return undefined
  }
})

const visibleMessage = computed(
  () =>
    (availableMessage.value?.alwaysShow || nodeHovered.value || selected.value) &&
    availableMessage.value,
)

const nodeHovered = ref(false)

const selected = computed(() => nodeSelection?.isSelected(nodeId.value) ?? false)

const isOnlyOneSelected = computed(
  () =>
    nodeSelection?.committedSelection.size === 1 &&
    nodeSelection?.committedSelection.has(nodeId.value),
)

const menuVisible = computed(() => menuEnabledByHover.value || isOnlyOneSelected.value)
const menuFull = ref(false)
const menuHovered = ref(false)

function eventScenePos(event: MouseEvent) {
  const clientPos = event && new Vec2(event.clientX, event.clientY)
  return clientPos && navigator?.clientToScenePos(clientPos)
}

const nodeHoverPos = ref<Vec2>()
const selectionHoverPos = ref<Vec2>()
function updateNodeHover(event: PointerEvent | undefined) {
  nodeHoverPos.value = event && eventScenePos(event)
}

const menuCloseTimeout = ref<ReturnType<typeof setTimeout>>()
const menuEnabledByHover = ref(false)
watchEffect(() => {
  if (menuCloseTimeout.value != null) {
    clearTimeout(menuCloseTimeout.value)
    menuCloseTimeout.value = undefined
  }
  const inZone = (pos: Vec2 | undefined) =>
    pos != null &&
    pos.sub(nodePosition.value).x <
      CONTENT_PADDING + ICON_WIDTH + GRAB_HANDLE_X_MARGIN_L + GRAB_HANDLE_X_MARGIN_R
  const hovered =
    menuHovered.value ||
    inZone(nodeHoverPos.value) ||
    (menuEnabledByHover.value && inZone(selectionHoverPos.value))
  if (hovered) {
    menuEnabledByHover.value = true
  } else if (!hovered && menuEnabledByHover.value) {
    menuCloseTimeout.value = setTimeout(() => {
      menuEnabledByHover.value =
        menuHovered.value || inZone(nodeHoverPos.value) || inZone(selectionHoverPos.value)
    }, MENU_CLOSE_TIMEOUT_MS)
  }
})

watch(menuVisible, (visible) => {
  if (!visible) menuFull.value = false
})

function setSoleSelected() {
  nodeSelection?.setSelection(new Set([nodeId.value]))
  graph.db.moveNodeToTop(nodeId.value)
}

function ensureSelected() {
  if (!nodeSelection?.isSelected(nodeId.value)) {
    setSoleSelected()
  }
}

const outputHovered = ref(false)
const keyboard = injectKeyboard()

const visualizationWidth = computed(() => props.node.vis?.width ?? null)
const visualizationHeight = computed(() => props.node.vis?.height ?? null)
const isVisualizationEnabled = computed({
  get: () => props.node.vis?.visible ?? false,
  set: (enabled) => {
    emit('update:visualizationEnabled', enabled)
  },
})
const isVisualizationPreviewed = computed(
  () => keyboard.mod && outputHovered.value && !isVisualizationEnabled.value,
)
const isVisualizationVisible = computed(
  () => isVisualizationEnabled.value || isVisualizationPreviewed.value,
)
watch(isVisualizationPreviewed, (newVal, oldVal) => {
  if (newVal && !oldVal) {
    graph.db.moveNodeToTop(nodeId.value)
  }
})

const transform = computed(() => {
  const { x, y } = nodePosition.value
  return `translate(${x}px, ${y}px)`
})

const startEpochMs = ref(0)
const significantMove = ref(false)

const dragPointer = usePointer(
  (pos, event, type) => {
    if (type !== 'start') {
      if (
        !significantMove.value &&
        (Number(new Date()) - startEpochMs.value >= MAXIMUM_CLICK_LENGTH_MS ||
          pos.relative.lengthSquared() >= MAXIMUM_CLICK_DISTANCE_SQ)
      ) {
        // If this is clearly a drag (not a click), the node itself capture pointer events to
        // prevent `click` on widgets.
        if (event.currentTarget instanceof Element)
          event.currentTarget.setPointerCapture?.(event.pointerId)
        significantMove.value = true
      }
      const fullOffset = pos.relative
      emit('dragging', fullOffset)
    }
    switch (type) {
      case 'start':
        startEpochMs.value = Number(new Date())
        significantMove.value = false
        break
      case 'stop':
        startEpochMs.value = 0
        emit('draggingCommited')
        break
      case 'cancel':
        startEpochMs.value = 0
        emit('draggingCancelled')
        break
    }
  },
  // Pointer is captured by `target`, to make it receive the `up` and `click` event in case this
  // is not going to be a node drag.
  { pointerCapturedBy: 'target' },
)
const isDragged = computed(() => dragPointer.dragging && significantMove.value)
watch(isDragged, () => graph.db.moveNodeToTop(nodeId.value))

const isRecordingOverridden = computed({
  get() {
    return props.node.prefixes.enableRecording != null
  },
  set(shouldOverride) {
    const edit = props.node.rootExpr.module.edit()
    const replacement =
      shouldOverride && !projectStore.isRecordingEnabled ?
        [Ast.TextLiteral.new(projectStore.executionMode, edit)]
      : undefined
    prefixes.modify(edit.getVersion(props.node.rootExpr), { enableRecording: replacement })
    graph.commitEdit(edit)
  },
})

const expressionInfo = computed(() => graph.db.getExpressionInfo(props.node.innerExpr.externalId))
const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
const color = computed(() => graph.db.getNodeColorStyle(nodeId.value))

const nodeEditHandler = nodeEditBindings.handler({
  cancel(e) {
    if (e.target instanceof HTMLElement) {
      e.target.blur()
    }
  },
  edit() {
    startEditingNode()
  },
})

function startEditingNode() {
  emit('update:edited', props.node.rootExpr.code().length)
}

const handleNodeClick = useDoubleClick(
  (e: MouseEvent) => {
    if (!significantMove.value) {
      nodeSelection?.handleSelectionOf(e, new Set([nodeId.value]))
      nodeEditHandler(e)
    }
  },
  () => {
    if (!significantMove.value) emit('enterNode')
  },
).handleClick

const visRect = shallowRef<Rect>()
function updateVisualizationRect(rect: Rect | undefined) {
  visRect.value = rect
  emit('update:visualizationRect', rect)
}

const graphSelectionSize = computed(() =>
  isVisualizationEnabled.value && visRect.value ? visRect.value.size : nodeSize.value,
)

const nodeRect = computed(() => new Rect(props.node.position, nodeSize.value))
const nodeOuterRect = computed(() =>
  isVisualizationEnabled.value && visRect.value ? visRect.value : nodeRect.value,
)
watchEffect(() => {
  if (!nodeOuterRect.value.size.isZero()) {
    emit('update:rect', nodeOuterRect.value)
  }
})

const dataSource = computed(
  () => ({ type: 'node', nodeId: props.node.rootExpr.externalId }) as const,
)

const pending = computed(() => {
  switch (executionState.value) {
    case 'Unknown':
    case 'Pending':
      return true
    default:
      return false
  }
})

// === Recompute node expression ===

function useRecomputation() {
  // The node is considered to be recomputing for at least this time.
  const MINIMAL_EXECUTION_TIMEOUT_MS = 500
  const recomputationTimeout = ref(false)
  const actualRecomputationStatus = nodeExecution.isBeingRecomputed(nodeId.value)
  const isBeingRecomputed = computed(
    () => recomputationTimeout.value || actualRecomputationStatus.value,
  )
  function recomputeOnce() {
    nodeExecution.recomputeOnce(nodeId.value, 'Live')
    recomputationTimeout.value = true
    setTimeout(() => (recomputationTimeout.value = false), MINIMAL_EXECUTION_TIMEOUT_MS)
  }
  return { recomputeOnce, isBeingRecomputed }
}

const nodeStyle = computed(() => {
  return {
    transform: transform.value,
    minWidth: isVisualizationEnabled.value ? `${visualizationWidth.value ?? 200}px` : undefined,
    '--node-group-color': color.value,
    ...(props.node.zIndex ? { 'z-index': props.node.zIndex } : {}),
    '--viz-below-node': `${graphSelectionSize.value.y - nodeSize.value.y}px`,
    '--node-size-x': `${nodeSize.value.x}px`,
    '--node-size-y': `${nodeSize.value.y}px`,
  }
})

const nodeClass = computed(() => {
  return {
    selected: selected.value,
    pending: pending.value,
    inputNode: props.node.type === 'input',
    outputNode: props.node.type === 'output',
    menuVisible: menuVisible.value,
    menuFull: menuFull.value,
  }
})

// === Component actions ===

const { getNodeColor, getNodeColors } = injectNodeColors()
const { recomputeOnce, isBeingRecomputed } = useRecomputation()

const { editingComment } = provideComponentButtons(
  {
    graphBindings: graphBindings.bindings,
    nodeEditBindings: nodeEditBindings.bindings,
    onBeforeAction: setSoleSelected,
  },
  {
    enterNode: {
      action: () => emit('enterNode'),
      hidden: computed(() => !graph.nodeCanBeEntered(nodeId.value)),
    },
    startEditing: {
      action: startEditingNode,
    },
    editingComment: {
      state: ref(false),
    },
    createNewNode: {
      action: () => emit('createNodes', [{ commit: false, content: undefined }]),
    },
    toggleDocPanel: {
      action: () => emit('toggleDocPanel'),
    },
    toggleVisualization: {
      state: isVisualizationEnabled,
    },
    pickColor: {
      state: ref(false),
      actionData: {
        currentColor: computed({
          get: () => getNodeColor(nodeId.value),
          set: (color) => emit('setNodeColor', color),
        }),
        matchableColors: getNodeColors((node) => node !== nodeId.value),
      },
    },
    recompute: {
      action: recomputeOnce,
      disabled: isBeingRecomputed,
    },
  },
)

const showMenuAt = ref<{ x: number; y: number }>()
</script>

<template>
  <div
    v-show="!edited"
    ref="rootNode"
    class="GraphNode define-node-colors"
    :style="nodeStyle"
    :class="nodeClass"
    :data-node-id="nodeId"
    @pointerenter="((nodeHovered = true), updateNodeHover($event))"
    @pointerleave="((nodeHovered = false), updateNodeHover(undefined))"
    @pointermove="updateNodeHover"
  >
    <div class="binding" v-text="node.pattern?.code()" />
    <button
      v-if="!menuVisible && isRecordingOverridden"
      class="overrideRecordButton clickable"
      data-testid="recordingOverriddenButton"
      @click="((isRecordingOverridden = false), setSoleSelected())"
    >
      <SvgIcon name="record" />
    </button>
    <ComponentMenu
      v-if="menuVisible"
      @pointerenter="menuHovered = true"
      @pointerleave="menuHovered = false"
      @click.capture="setSoleSelected"
    />
    <GraphVisualization
      v-if="isVisualizationVisible"
      :nodeSize="nodeSize"
      :scale="navigator?.scale ?? 1"
      :nodePosition="nodePosition"
      :isComponentMenuVisible="menuVisible"
      :currentType="props.node.vis?.identifier"
      :dataSource="dataSource"
      :typename="expressionInfo?.typename"
      :width="visualizationWidth"
      :height="visualizationHeight"
      :isFocused="isOnlyOneSelected"
      :isPreview="isVisualizationPreviewed"
      :isFullscreenAllowed="true"
      :isResizable="true"
      @update:rect="updateVisualizationRect"
      @update:id="emit('update:visualizationId', $event)"
      @update:enabled="emit('update:visualizationEnabled', $event)"
      @update:width="emit('update:visualizationWidth', $event)"
      @update:height="emit('update:visualizationHeight', $event)"
      @update:nodePosition="graph.setNodePosition(nodeId, $event)"
      @createNodes="emit('createNodes', $event)"
      @click.capture="setSoleSelected"
    />
    <GraphNodeComment
      v-model:editing="editingComment.state"
      :node="node"
      class="beforeNode"
      @click.capture="setSoleSelected"
    />
    <div
      ref="contentNode"
      :class="{ content: true, dragged: isDragged }"
      :style="contentNodeStyle"
      v-on="dragPointer.events"
      @click="handleNodeClick"
      @contextmenu.stop.prevent="(ensureSelected(), (showMenuAt = $event))"
    >
      <ComponentWidgetTree
        :ast="props.node.innerExpr"
        :nodeId="nodeId"
        :rootElement="rootNode"
        :nodeType="props.node.type"
        :potentialSelfArgumentId="potentialSelfArgumentId"
        :conditionalPorts="props.node.conditionalPorts"
        :extended="isOnlyOneSelected"
      />
    </div>
    <div class="statuses">
      <SvgIcon
        v-if="availableMessage && !visibleMessage"
        :name="iconForMessageType[availableMessage.type]"
        :style="{ color: colorForMessageType[availableMessage.type] }"
      />
    </div>
    <GraphNodeMessage
      v-if="visibleMessage"
      class="afterNode shiftWhenMenuVisible"
      :message="visibleMessage.text"
      :type="visibleMessage.type"
    />
    <svg class="bgPaths">
      <rect class="bgFill" />
      <GraphNodeOutputPorts
        v-if="props.node.type !== 'output'"
        :nodeId="nodeId"
        :forceVisible="nodeHovered"
        @newNodeClick="
          (setSoleSelected(), emit('createNodes', [{ commit: false, content: undefined }]))
        "
        @portClick="(...args) => emit('outputPortClick', ...args)"
        @portDoubleClick="(...args) => emit('outputPortDoubleClick', ...args)"
        @update:hoverAnim="emit('update:hoverAnim', $event)"
        @update:nodeHovered="outputHovered = $event"
      />
    </svg>
  </div>
  <PointFloatingMenu v-if="showMenuAt" :point="showMenuAt" @close="showMenuAt = undefined">
    <ComponentContextMenu @close="showMenuAt = undefined" />
  </PointFloatingMenu>
</template>

<style scoped>
.bgPaths {
  width: 100%;
  height: 100%;
  position: absolute;
  overflow: visible;
  top: 0;
  left: 0;
  display: flex;
  --output-port-transform: translateY(var(--viz-below-node));
}

.bgFill {
  width: var(--node-size-x);
  height: var(--node-size-y);
  rx: var(--node-border-radius);

  fill: var(--color-node-background);
  transition: fill 0.2s ease;
}

.GraphNode {
  position: absolute;
  border-radius: var(--node-border-radius);
  transition: box-shadow 0.2s ease-in-out;
  box-sizing: border-box;
}

.content {
  font-family: var(--font-code);
  position: relative;
  top: 0;
  left: 0;
  border-radius: var(--node-border-radius);
  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  z-index: 24;
  transition: outline 0.2s ease;
  outline: 0px solid transparent;
}

.binding {
  font-family: var(--font-code);
  user-select: none;
  pointer-events: none;
  margin-right: 10px;
  color: black;
  position: absolute;
  right: 100%;
  top: 50%;
  transform: translateY(-50%);
  opacity: 0;
  transition: opacity 0.2s ease-in-out;
  white-space: nowrap;
}

.selected .binding {
  opacity: 1;
}

.ComponentMenu {
  z-index: 25;
  &.partial {
    z-index: 1;
  }
}

.beforeNode {
  position: absolute;
  bottom: 100%;
  width: calc(max(100%, 800px));
  max-width: max-content;
  margin-bottom: var(--node-vertical-gap);
  /* Allow space for the input arrow. */
  left: 24px;
  transition: left 0.1s ease-out;
}
.menuFull .beforeNode {
  left: 64px;
}

.afterNode {
  position: absolute;
  top: 100%;
  margin-top: var(--node-vertical-gap);
  transform: translateY(var(--viz-below-node));
}
.shiftWhenMenuVisible {
  left: 0;
  transition: left 0.1s ease-out;
}
.menuVisible .shiftWhenMenuVisible {
  left: 40px;
}

.belowMenu {
  position: absolute;
  top: calc(100% + 40px);
}

.statuses {
  position: absolute;
  pointer-events: none;
  display: flex;
  align-items: center;
  gap: 4px;
  height: 100%;
  top: 0;
  right: 100%;
  margin-right: 8px;
  transition: opacity 0.2s ease-in-out;
}

.GraphNode.selected .statuses {
  opacity: 0;
}

.overrideRecordButton {
  position: absolute;
  display: flex;
  align-items: center;
  backdrop-filter: var(--blur-app-bg);
  background: var(--color-app-bg);
  border-radius: var(--radius-full);
  color: red;
  padding: 8px;
  height: 100%;
  right: 100%;
  margin-right: 4px;
}

.dragged {
  cursor: grabbing !important;
}
</style>
