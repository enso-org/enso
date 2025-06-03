<script lang="ts">
const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50
export const NODE_CONTENT_PADDING = 4
export const NODE_CONTENT_PADDING_PX = `${NODE_CONTENT_PADDING}px`
const MENU_CLOSE_TIMEOUT_MS = 300
</script>

<script setup lang="ts">
import { useGraphStore, useProjectStore } from '$/components/WithCurrentProject.vue'
import { nodeEditBindings } from '@/bindings'
import ComponentMenu from '@/components/ComponentMenu.vue'
import ContextMenuTrigger from '@/components/ContextMenuTrigger.vue'
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
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SvgIcon from '@/components/SvgIcon.vue'
import { useComponentColors } from '@/composables/componentColors'
import { useDoubleClick } from '@/composables/doubleClick'
import { usePointer, useResizeObserver } from '@/composables/events'
import { useProgressBackground } from '@/composables/progressBar'
import type { ActionHandler } from '@/providers/action'
import { registerHandlers, toggledAction } from '@/providers/action'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectKeyboard } from '@/providers/keyboard'
import { provideResizableWidgetRegistry } from '@/providers/resizableWidgetRegistry'
import { type Node } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
import { evaluationProgress } from '@/stores/project/computedValueRegistry'
import { useNodeExecution } from '@/stores/project/nodeExecution'
import { Ast } from '@/util/ast'
import { prefixes } from '@/util/ast/node'
import { onWindowBlur } from '@/util/autoBlur'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { ComponentInstance, computed, onUnmounted, ref, shallowRef, watch, watchEffect } from 'vue'
import type { ExternalId, VisualizationIdentifier } from 'ydoc-shared/yjsModel'

const contentNodeStyle = {
  padding: NODE_CONTENT_PADDING_PX,
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
  enterNode: []
  createNodes: [options: NodeCreationOptions[]]
  setNodeColor: [color: string | undefined]
  toggleDocPanel: []
  'update:edited': [cursorPosition: number]
  'update:rect': [rect: Rect]
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
const primaryApplication = computed(() => props.node.primaryApplication)

const nodePosition = computed(() => {
  // Positions of nodes that are not yet placed are set to `Infinity`.
  if (props.node.position.equals(Vec2.Infinity)) return Vec2.Zero
  return props.node.position
})

onUnmounted(() => graph.unregisterNodeRect(nodeId.value))

const rootNode = ref<HTMLElement>()
const contentNode = ref<HTMLElement>()
const widgetTree = ref<ComponentInstance<typeof ComponentWidgetTree>>()
const nodeSize = useResizeObserver(rootNode)
const widgetTreeSize = useResizeObserver(widgetTree)

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

const nodeHovered = computed(() => graph.nodeHovered.get(nodeId.value) ?? false)

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
      NODE_CONTENT_PADDING + ICON_WIDTH + GRAB_HANDLE_X_MARGIN_L + GRAB_HANDLE_X_MARGIN_R
  const hovered =
    nodeHovered.value ||
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
}

function ensureSelected() {
  if (!nodeSelection?.isSelected(nodeId.value)) {
    setSoleSelected()
  }
}

const outputHovered = computed(() => graph.nodeOutputVisible.get(nodeId.value) ?? false)
const keyboard = injectKeyboard()

const visualizationWidth = computed({
  get: () => props.node.vis?.width ?? null,
  set: (value) => value && emit('update:visualizationWidth', value),
})
const visualizationHeight = computed(() => props.node.vis?.height ?? null)
const isVisualizationEnabled = computed({
  get: () => props.node.vis?.visible ?? false,
  set: (enabled) => {
    emit('update:visualizationEnabled', enabled)
  },
})
const visualizationHovered = ref(false)

const isVisualizationPreviewed = computed(
  () =>
    keyboard.mod &&
    (outputHovered.value || visualizationHovered.value || nodeHovered.value) &&
    !isVisualizationEnabled.value,
)
const isVisualizationVisible = computed(
  () => isVisualizationEnabled.value || isVisualizationPreviewed.value,
)
watch(isVisualizationVisible, (val) => {
  // When visualization is being hidden, we don’t receive `pointerleave` event for some reason.
  // So we need to set `visualizationHovered` to `false` manually.
  if (!val) {
    visualizationHovered.value = false
  }
})
watch(isVisualizationPreviewed, (newVal, oldVal) => {
  if (!newVal) {
    graph.setNodeHovered(nodeId.value, false)
  } else if (newVal && !oldVal) {
    graph.db.moveNodeToTop(nodeId.value)
  }
})

const scale = computed(() => navigator?.scale ?? 1)
provideResizableWidgetRegistry(
  computed({
    get: () => visualizationWidth.value && visualizationWidth.value * scale.value,
    set: (width) => (visualizationWidth.value = width && width / scale.value),
  }),
  () => NODE_CONTENT_PADDING * scale.value,
  () => widgetTreeSize.value.x,
)

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

const typename = computed(() => expressionInfo.value?.rawTypename)

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

// === Style and colors ===

const nodeStyle = computed(() => {
  return {
    transform: transform.value,
    minWidth: isVisualizationEnabled.value ? `${visualizationWidth.value ?? 200}px` : undefined,
    '--node-group-color': baseColor.value,
    ...(props.node.zIndex ? { 'z-index': props.node.zIndex } : {}),
    '--viz-below-node': `${graphSelectionSize.value.y - nodeSize.value.y}px`,
  }
})

const { baseColor, selected, pending } = useComponentColors(graph.db, nodeSelection, nodeId)

const nodeProgress = computed(() => evaluationProgress(expressionInfo.value) ?? 100)
const { progressStyles, watchProgress } = useProgressBackground(nodeProgress, {
  progressId: () => expressionInfo.value?.evaluationId ?? 0,
  initialColor: 'var(--color-node-background-pending)',
  finalColor: 'var(--color-node-background)',
})
const { progressAnimating, backgroundProgressEvents } = watchProgress()

const showProgressBar = computed(() => nodeProgress.value !== 100 || progressAnimating.value)

const nodeClass = computed(() => {
  return {
    selected: selected.value,
    pending: pending.value,
    evaluating: showProgressBar.value,
    inputNode: props.node.type === 'input',
    outputNode: props.node.type === 'output',
    menuVisible: menuVisible.value,
    menuFull: menuFull.value,
    edited: props.edited,
  }
})

const backgroundStyles = computed(() =>
  composeTransition(showProgressBar.value ? progressStyles.value : {}, [
    '--color-node-background 0.2s ease',
    '--color-node-background-pending 0.2s ease',
  ]),
)

/**
 * Returns the provided CSS style properties, with the provided additional transitions combined with any existing
 * `transition`.
 */
function composeTransition(style: Record<string, string>, additionalTransitions: string[]) {
  return {
    ...style,
    transition: (style.transition ?
      [style.transition, ...additionalTransitions]
    : additionalTransitions
    ).join(','),
  }
}

// === Component actions ===

const { getNodeColor, getNodeColors } = injectNodeColors()
const nodeColor = computed(() => getNodeColor(nodeId.value))
const matchableColors = getNodeColors((node) => node !== nodeId.value)
const { recomputeOnce, isBeingRecomputed } = useRecomputation()

function selectBeforeAction<Handlers extends { [K in string]?: ActionHandler }>(
  handlers: Handlers,
) {
  for (const actionName in handlers) {
    const origAction = handlers[actionName]!.action
    handlers[actionName]!.action = (...args) => {
      setSoleSelected()
      origAction?.(...args)
    }
  }
  return handlers
}

const editingComment = ref(false)
const colorPickerOpened = ref(false)

registerHandlers(
  selectBeforeAction({
    'component.enterNode': {
      action: () => emit('enterNode'),
      hidden: computed(() => !graph.nodeCanBeEntered(nodeId.value)),
    },
    'component.startEditing': {
      action: startEditingNode,
    },
    'component.editingComment': toggledAction(editingComment),
    'component.createNewNode': {
      action: () => emit('createNodes', [{ commit: false, content: undefined }]),
    },
    'component.toggleDocPanel': {
      action: () => emit('toggleDocPanel'),
    },
    'component.toggleVisualization': toggledAction(isVisualizationEnabled),
    'component.pickColor': toggledAction(colorPickerOpened),
    'component.recompute': {
      action: recomputeOnce,
      disabled: isBeingRecomputed,
    },
  }),
)

onWindowBlur(() => {
  graph.setNodeHovered(nodeId.value, false)
  updateNodeHover(undefined)
})

const nodeName = computed(() => props.node.pattern?.code())
</script>

<template>
  <div
    ref="rootNode"
    class="GraphNode define-node-colors"
    :style="nodeStyle"
    :class="nodeClass"
    :data-node-id="nodeId"
    @pointerdown.stop
  >
    <div class="binding" v-text="nodeName" />
    <button
      v-if="!menuVisible && isRecordingOverridden"
      class="overrideRecordButton clickable"
      data-testid="recordingOverriddenButton"
      @click="((isRecordingOverridden = false), setSoleSelected())"
    >
      <SvgIcon name="workflow_play" />
    </button>
    <ComponentMenu
      v-if="menuVisible"
      :colorPickerOpened="colorPickerOpened"
      :currentNodeColor="nodeColor"
      :matchableColors="matchableColors"
      @setNodeColor="emit('setNodeColor', $event)"
      @closeColorPicker="colorPickerOpened = false"
      @pointerenter="menuHovered = true"
      @pointerleave="menuHovered = false"
      @click.capture="setSoleSelected"
    />
    <GraphVisualization
      v-if="isVisualizationVisible"
      v-model:width="visualizationWidth"
      :nodeSize="nodeSize"
      :scale="navigator?.scale ?? 1"
      :nodePosition="nodePosition"
      :isComponentMenuVisible="menuVisible"
      :currentType="props.node.vis?.identifier"
      :dataSource="dataSource"
      :typename="typename"
      :height="visualizationHeight"
      :isFocused="isOnlyOneSelected"
      :isPreview="isVisualizationPreviewed"
      :isFullscreenAllowed="true"
      :isResizable="true"
      @pointerenter="visualizationHovered = true"
      @pointerleave="visualizationHovered = false"
      @update:rect="updateVisualizationRect"
      @update:id="emit('update:visualizationId', $event)"
      @update:enabled="emit('update:visualizationEnabled', $event)"
      @update:height="emit('update:visualizationHeight', $event)"
      @update:nodePosition="graph.setNodePosition(nodeId, $event)"
      @createNodes="emit('createNodes', $event)"
      @click.capture="setSoleSelected"
    />
    <GraphNodeComment
      v-model:editing="editingComment"
      :node="node"
      class="beforeNode"
      @click.capture="setSoleSelected"
    />
    <ContextMenuTrigger
      :actions="[
        'component.toggleDocPanel',
        'component.toggleVisualization',
        'component.createNewNode',
        'component.editingComment',
        'component.recompute',
        'component.pickColor',
        'component.enterNode',
        'component.startEditing',
        'components.copy',
        'components.deleteSelected',
      ]"
      @contextmenu="ensureSelected"
    >
      <div
        ref="contentNode"
        :class="{ content: true, dragged: isDragged }"
        :style="contentNodeStyle"
        v-on="dragPointer.events"
        @click="handleNodeClick"
        @pointerenter="(graph.setNodeHovered(nodeId, true), updateNodeHover($event))"
        @pointerleave="(graph.setNodeHovered(nodeId, false), updateNodeHover(undefined))"
        @pointermove="updateNodeHover"
      >
        <ComponentWidgetTree
          ref="widgetTree"
          :ast="props.node.innerExpr"
          :nodeId="nodeId"
          :rootElement="rootNode"
          :nodeType="props.node.type"
          :primaryApplication="primaryApplication"
          :conditionalPorts="props.node.conditionalPorts"
          :extended="isOnlyOneSelected"
        />
      </div>
    </ContextMenuTrigger>
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
      :outputPortHovered="outputHovered"
    />
    <div class="nodeBackground" :style="backgroundStyles" v-on="backgroundProgressEvents"></div>
  </div>
</template>

<style scoped>
.GraphNode {
  position: absolute;
  border-radius: var(--node-border-radius);
  transition: box-shadow 0.2s ease-in-out;
  box-sizing: border-box;
}

.nodeBackground {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  border-radius: var(--node-border-radius);
  background-color: var(--color-node-background);
  transition: background-color 0.2s ease;
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
}

.binding {
  font-family: var(--font-code);
  user-select: none;
  pointer-events: none;
  margin-right: 10px;
  color: black;
  position: absolute;
  right: 100%;
  top: 0;
  bottom: 0;
  opacity: 0;
  transition: opacity 0.2s ease-in-out;
  white-space: nowrap;
  display: flex;
  align-items: center;
}

.selected .binding {
  opacity: 1;
}

.ComponentMenu {
  z-index: 20;
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
  transition:
    left 0.1s ease-out,
    opacity 0.2s ease;
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

/* We use this instead of "v-show", because we want the node content being still laid out,
   so the edges won't jump. */
.edited {
  visibility: hidden;
}
</style>
