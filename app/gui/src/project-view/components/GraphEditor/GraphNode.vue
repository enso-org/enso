<script lang="ts">
export const NODE_CONTENT_PADDING = 4
export const NODE_CONTENT_PADDING_PX = `${NODE_CONTENT_PADDING}px`
const MENU_CLOSE_TIMEOUT_MS = 300
</script>

<script setup lang="ts">
import {
  useCurrentProject,
  useGraphStore,
  useProjectStore,
} from '$/components/WithCurrentProject.vue'
import { type Node } from '$/providers/openedProjects/graph'
import { asNodeId } from '$/providers/openedProjects/graph/graphDatabase'
import { evaluationProgress } from '$/providers/openedProjects/project/computedValueRegistry'
import { useNodeExecution } from '$/providers/openedProjects/project/nodeExecution'
import { nodeEditBindings } from '@/bindings'
import ComponentMenu from '@/components/ComponentMenu.vue'
import ContextMenuTrigger from '@/components/ContextMenuTrigger.vue'
import ComponentWidgetTree, {
  GRAB_HANDLE_X_MARGIN_L,
  GRAB_HANDLE_X_MARGIN_R,
  ICON_WIDTH,
} from '@/components/GraphEditor/ComponentWidgetTree.vue'
import { useNodeMessage } from '@/components/GraphEditor/GraphNode/nodeMessage'
import { useNodeVisualization } from '@/components/GraphEditor/GraphNode/nodeVisualization'
import GraphNodeComment from '@/components/GraphEditor/GraphNodeComment.vue'
import GraphNodeMessage from '@/components/GraphEditor/GraphNodeMessage.vue'
import GraphNodeSubmenu from '@/components/GraphEditor/GraphNodeSubmenu.vue'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { useNodesDisplacing } from '@/components/GraphEditor/nodesDisplacing'
import { useResizeHandles } from '@/components/resizeHandles'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useComponentColors } from '@/composables/componentColors'
import { useClickableDraggable } from '@/composables/dragging'
import { useResizeObserver } from '@/composables/events'
import { useProgressBackground } from '@/composables/progressBar'
import type { ActionHandler, DisplayableActionName } from '@/providers/action'
import { registerHandlers, toggledAction } from '@/providers/action'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { useGraphSelection } from '@/providers/graphSelection'
import { providePopoverRoot } from '@/providers/popoverRoot'
import { provideWidgetControlledActions } from '@/providers/widgetActions'
import { Ast } from '@/util/ast'
import { prefixes } from '@/util/ast/node'
import { onWindowBlur } from '@/util/autoBlur'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { Ok } from 'enso-common/src/utilities/data/result'
import { computed, onUnmounted, ref, toRef, watch, watchEffect } from 'vue'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

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
  'update:height': [height: number | undefined]
  'update:visualizationId': [id: Opt<VisualizationIdentifier>]
  'update:visualizationEnabled': [enabled: boolean]
  'update:visualizationWidth': [width: number]
  'update:visualizationHeight': [height: number]
}>()

const nodeSelection = useGraphSelection(true)
const projectStore = useProjectStore()
const graph = useGraphStore()
const { module } = useCurrentProject()
const navigator = injectGraphNavigator(true)
const nodeExecution = useNodeExecution()

const nodeId = computed(() => asNodeId(props.node.rootExpr.externalId))
const primaryApplication = computed(() => props.node.primaryApplication)

const scale = computed(() => navigator?.scale ?? 1)

const nodePosition = computed(() => {
  // Positions of nodes that are not yet placed are set to `Infinity`.
  if (props.node.position.equals(Vec2.Infinity)) return Vec2.Zero
  return props.node.position
})

onUnmounted(() => graph.unregisterNodeRect(nodeId.value))

const rootNode = ref<HTMLElement>()
const widgetTreeNode = ref<HTMLElement>()

const widgetsDomSizeClientPx = useResizeObserver(widgetTreeNode, false)
const widgetsDomSize = ref(new Vec2(0, 0))
// Maintain the size in scene px. The values reported by the resize observer are in client px, so they are dependent on
// the scale; however, changes to the scale don't cause resize events--so the resize observer is non-reactively (via the
// DOM) dependent on reactive state. Thus, we must correct for the scale by non-reactively sampling it at the time a
// resize is observed.
watch(widgetsDomSizeClientPx, (size) => (widgetsDomSize.value = size.scale(1 / scale.value)), {
  immediate: true,
  flush: 'sync',
})
// Compute the node's natural size based on the size of its widgets. We measure the widget tree instead of the node
// directly, because measuring the node would cause a cycle:
// - This value is used as in input to determine the size of the visualization.
// - The size of the visualization affects the size of the node.
const nodeDomSize = computed(() =>
  widgetsDomSize.value.add(new Vec2(NODE_CONTENT_PADDING * 2, NODE_CONTENT_PADDING * 2)),
)

providePopoverRoot(rootNode)

const { visibleMessage, hiddenMessage } = useNodeMessage({
  projectStore,
  graphDb: graph.db,
  expand: () => nodeHovered.value || selected.value,
  nodeId,
})

const detailedView = computed<boolean>(
  () => nodeSelection != null && nodeSelection.isSoleSelection(nodeId.value),
)
watch(detailedView, (extended) => graph.nodeDetailedView.set(nodeId.value, extended), {
  immediate: true,
})

const expanded = toRef(() => props.node.isExpanded)

const nodeHovered = ref(false)
watch(nodeHovered, (hovered) => graph.nodeHovered.set(nodeId.value, hovered))

const menuVisible = computed(() => menuEnabledByHover.value || detailedView.value)
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

const outputHovered = computed(() => graph.nodeOutputHovered.get(nodeId.value))

const { displaceNodesForResize } = useNodesDisplacing()
const {
  visualizationWidth,
  isVisualizationEnabled,
  isVisualizationPreviewed,
  vizHeight,
  visualization,
} = useNodeVisualization({
  vis: () => props.node.vis,
  nodeHovered: () => nodeHovered.value || outputHovered.value,
  nodeWidgetsSize: nodeDomSize,
  nodePos: () => props.node.position,
  scale,
  isFocused: detailedView,
  typeinfo: () => expressionInfo.value?.typeInfo,
  dataSource: () => ({ type: 'node', nodeId: props.node.rootExpr.externalId }) as const,
  hidden: toRef(props, 'edited'),
  emit,
  onResize: (rect0, rect1) => displaceNodesForResize(nodeId.value, rect0, rect1),
})

watch(isVisualizationPreviewed, (newVal) => {
  if (newVal) {
    graph.db.moveNodeToTop(nodeId.value)
  } else {
    graph.nodeHovered.delete(nodeId.value)
  }
})

const transform = computed(() => {
  const { x, y } = nodePosition.value
  return `translate(${x}px, ${y}px)`
})

const { isDragged, pointerEvents } = useClickableDraggable({
  dragMove: (fullOffset) => emit('dragging', fullOffset),
  dragCommit: () => emit('draggingCommited'),
  dragCancel: () => emit('draggingCancelled'),
  click: (e: MouseEvent) => {
    nodeSelection?.handleSelectionOf(e, new Set([nodeId.value]))
    nodeEditHandler(e)
  },
  doubleClick: () => emit('enterNode'),
})
watch(isDragged, () => graph.db.moveNodeToTop(nodeId.value))

const isRecordingOverridden = computed({
  get() {
    return props.node.prefixes.enableRecording != null
  },
  set(shouldOverride) {
    module.value.edit((edit) => {
      const replacement =
        shouldOverride && !projectStore.isRecordingEnabled ?
          [Ast.TextLiteral.new(projectStore.executionMode, edit)]
        : undefined
      prefixes.value.modify(edit.getVersion(props.node.rootExpr), { enableRecording: replacement })
      return Ok()
    })
  },
})

const expressionInfo = computed(() => graph.db.getExpressionInfo(props.node.innerExpr.externalId))

const nodeEditHandler = nodeEditBindings.handler({
  edit: () => actionHandlers['component.startEditing'].action(),
})

let prevNodeRect: Rect | undefined = undefined
watchEffect(() => {
  if (nodeDomSize.value.isZero()) return
  const width = Math.max(nodeDomSize.value.x, visualizationWidth.value)
  const height = nodeDomSize.value.y + vizHeight.value
  const newRect = new Rect(props.node.position, new Vec2(width, height))
  if (!prevNodeRect?.equals(newRect)) {
    emit('update:rect', newRect)
    prevNodeRect = newRect
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

// === Style and colors ===

/**
 * Node height: A node is auto-sized unless the user has resized it. When it is auto-sized, its height is determined by
 * DOM/CSS. Each resizable widget has a preferred size that is used as its `min-height`, with the effect that the node
 * takes the size of the largest resizable widget present. If the user resizes the node, and the node is in expanded
 * mode, the specified height overrides any widget preferences.
 */
const nodeHeightOverride = computed(() => props.node.height)
const nodeHeightOverridden = computed(() => props.node.height != null)
const nodeStyle = computed(() => {
  return {
    transform: transform.value,
    minWidth: `${visualizationWidth.value ?? 200}px`,
    height: nodeHeightOverride.value ? `${nodeHeightOverride.value}px` : undefined,
    '--node-group-color': baseColor.value,
    ...(props.node.zIndex ? { 'z-index': props.node.zIndex } : {}),
    '--viz-below-node': `${vizHeight.value}px`,
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

const nodeClass = computed<Record<string, boolean>>(() => {
  return {
    selected: selected.value,
    pending: pending.value,
    evaluating: showProgressBar.value,
    inputNode: props.node.type === 'input',
    outputNode: props.node.type === 'output',
    menuVisible: menuVisible.value,
    menuFull: menuFull.value,
    edited: props.edited,
    nodeHeightOverridden: nodeHeightOverridden.value,
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
const isExpanded = computed({
  get: () => props.node.isExpanded,
  set: (value) => graph.setNodeDisplayMode(nodeId.value, value ? 'expanded' : 'collapsed'),
})

const actionHandlers = registerHandlers(
  selectBeforeAction({
    'component.enterNode': {
      available: computed(() => graph.nodeCanBeEntered(nodeId.value)),
      action: () => emit('enterNode'),
    },
    'component.startEditing': {
      action: () => emit('update:edited', props.node.rootExpr.code().length),
    },
    'component.editingComment': toggledAction(editingComment),
    'component.createNewNode': {
      action: () => emit('createNodes', [{ commit: false, content: undefined }]),
    },
    'component.toggleDocPanel': {
      action: () => emit('toggleDocPanel'),
    },
    'component.toggleVisualization': {
      ...toggledAction(isVisualizationEnabled),
      description: computed(() =>
        isVisualizationEnabled.value ? 'Hide visualization' : 'Show visualization',
      ),
    },
    'component.toggleExpanded': {
      ...toggledAction(isExpanded),
      description: computed(() => (isExpanded.value ? 'Collapse Component' : 'Expand Component')),
    },
    'component.pickColor': toggledAction(colorPickerOpened),
    'component.recompute': {
      enabled: computed(() => !isBeingRecomputed.value),
      action: recomputeOnce,
    },
    ...provideWidgetControlledActions(['component.widget.editMethodName']),
  }),
)

const nodeMenuActions: DisplayableActionName[] = [
  'component.toggleDocPanel',
  'component.toggleVisualization',
  'component.createNewNode',
  'component.editingComment',
  'component.toggleExpanded',
  'component.recompute',
  'component.pickColor',
  'component.enterNode',
  'component.widget.editMethodName',
  'component.startEditing',
  'components.copy',
  'components.deleteSelected',
  'components.deleteAndConnectAround',
]

const multiSelectionMenuActions: DisplayableActionName[] = [
  'components.collapse',
  'components.pickColorMulti',
  'components.copy',
  'components.deleteSelected',
]

const alignmentMenuActions: DisplayableActionName[] = [
  'components.alignLeft',
  'components.alignCenter',
  'components.alignRight',
  'components.alignTop',
  'components.alignBottom',
]

const spacingMenuActions: DisplayableActionName[] = [
  'components.spaceVertical',
  'components.spaceVerticalTight',
  'components.spaceVerticalZero',
  'components.spaceVerticalWide',
]

const selectionSize = computed(() => nodeSelection?.selected.size ?? 0)
const hasMultiSelection = computed(() => selectionSize.value > 1)

const contextMenuActions = computed<DisplayableActionName[]>(() =>
  hasMultiSelection.value ? multiSelectionMenuActions : nodeMenuActions,
)

onWindowBlur(() => {
  graph.nodeHovered.delete(nodeId.value)
  updateNodeHover(undefined)
})

const nodeName = computed(() => props.node.pattern?.code())

// === Node resizing ===

const resizeHandles = useResizeHandles({
  size: nodeDomSize,
  scale,
})
resizeHandles.onResizeHeight((value) => emit('update:height', value))
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
      :actions="nodeMenuActions"
      @setNodeColor="emit('setNodeColor', $event)"
      @closeColorPicker="colorPickerOpened = false"
      @update:hovered="menuHovered = $event"
      @click.capture="setSoleSelected"
    />
    <GraphVisualization
      v-bind="visualization"
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
    <ContextMenuTrigger :actions="contextMenuActions" @contextmenu="ensureSelected">
      <template #menuElements>
        <div v-if="hasMultiSelection">
          <GraphNodeSubmenu label="Align" icon="align_left" :actions="alignmentMenuActions" />
          <GraphNodeSubmenu label="Spacing" icon="space_default" :actions="spacingMenuActions" />
        </div>
      </template>
      <div
        :class="{ content: true, dragged: isDragged }"
        :style="contentNodeStyle"
        v-on="pointerEvents"
        @pointerenter="((nodeHovered = true), updateNodeHover($event))"
        @pointerleave="((nodeHovered = false), updateNodeHover(undefined))"
        @pointermove="updateNodeHover"
      >
        <ComponentWidgetTree
          ref="widgetTreeNode"
          :ast="props.node.innerExpr"
          :nodeId="nodeId"
          :rootElement="rootNode"
          :nodeType="props.node.type"
          :primaryApplication="primaryApplication"
          :conditionalPorts="props.node.conditionalPorts"
          :showDetails="detailedView"
          :expanded="expanded"
        />
        <ResizeHandles v-if="isExpanded" bottom v-on="resizeHandles.events" />
      </div>
    </ContextMenuTrigger>
    <div class="statuses">
      <SvgIcon v-if="hiddenMessage" v-bind="hiddenMessage" />
    </div>
    <GraphNodeMessage
      v-if="visibleMessage"
      v-bind="visibleMessage"
      class="afterNode shiftWhenMenuVisible"
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
  min-height: var(--node-base-height);
  --z-index-component: 24;
  --z-index-component-menu: 20;
  --z-index-selection-submenu: 25;
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
  /* Prevent this element from confusing Playwright's actionability checks */
  pointer-events: none;
}

.ComponentWidgetTree {
  height: 100%;
}

.content {
  font-family: var(--font-code);
  position: relative;
  top: 0;
  left: 0;
  height: 100%;
  border-radius: var(--node-border-radius);
  display: flex;
  flex-direction: row;
  align-items: start;
  white-space: nowrap;
  z-index: var(--z-index-component);
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

.menuVisible .binding {
  margin-right: 50px;
}

.selected .binding {
  opacity: 1;
}

.ComponentMenu {
  z-index: var(--z-index-component-menu);
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
  /*noinspection CssUnresolvedCustomProperty*/
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
