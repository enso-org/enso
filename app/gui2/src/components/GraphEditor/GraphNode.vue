<script setup lang="ts">
import { nodeEditBindings } from '@/bindings'
import CircularMenu from '@/components/CircularMenu.vue'
import GraphNodeComment from '@/components/GraphEditor/GraphNodeComment.vue'
import GraphNodeMessage, {
  colorForMessageType,
  iconForMessageType,
  type MessageType,
} from '@/components/GraphEditor/GraphNodeMessage.vue'
import GraphNodeSelection from '@/components/GraphEditor/GraphNodeSelection.vue'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import NodeWidgetTree, {
  GRAB_HANDLE_X_MARGIN,
  ICON_WIDTH,
} from '@/components/GraphEditor/NodeWidgetTree.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useApproach } from '@/composables/animation'
import { useDoubleClick } from '@/composables/doubleClick'
import { usePointer, useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore, type Node } from '@/stores/graph'
import { asNodeId } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import type { AstId } from '@/util/ast/abstract'
import { prefixes } from '@/util/ast/node'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { displayedIconOf } from '@/util/getIconName'
import { setIfUndefined } from 'lib0/map'
import type { ExternalId, VisualizationIdentifier } from 'shared/yjsModel'
import type { EffectScope } from 'vue'
import { computed, effectScope, onScopeDispose, onUnmounted, ref, watch, watchEffect } from 'vue'

const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50
const CONTENT_PADDING = 4
const CONTENT_PADDING_RIGHT = 8
const CONTENT_PADDING_PX = `${CONTENT_PADDING}px`
const CONTENT_PADDING_RIGHT_PX = `${CONTENT_PADDING_RIGHT}px`
const MENU_CLOSE_TIMEOUT_MS = 300

const props = defineProps<{
  node: Node
  edited: boolean
  graphNodeSelections: HTMLElement | undefined
}>()

const emit = defineEmits<{
  dragging: [offset: Vec2]
  draggingCommited: []
  delete: []
  replaceSelection: []
  outputPortClick: [event: PointerEvent, portId: AstId]
  outputPortDoubleClick: [event: PointerEvent, portId: AstId]
  doubleClick: []
  createNodes: [options: NodeCreationOptions[]]
  setNodeColor: [color: string]
  'update:edited': [cursorPosition: number]
  'update:rect': [rect: Rect]
  'update:visualizationId': [id: Opt<VisualizationIdentifier>]
  'update:visualizationRect': [rect: Rect | undefined]
  'update:visualizationVisible': [visible: boolean]
  'update:visualizationFullscreen': [fullscreen: boolean]
  'update:visualizationWidth': [width: number]
  'update:visualizationHeight': [height: number]
}>()

const nodeSelection = injectGraphSelection(true)
const projectStore = useProjectStore()
const graph = useGraphStore()
const navigator = injectGraphNavigator(true)

const outputPortsSet = computed(() => {
  const bindings = graph.db.nodeOutputPorts.lookup(nodeId.value)
  if (bindings.size === 0) return new Set([nodeId.value])
  return bindings
})

const nodeId = computed(() => asNodeId(props.node.rootExpr.id))
const potentialSelfArgumentId = computed(() => props.node.primarySubject)
const connectedSelfArgumentId = computed(() =>
  potentialSelfArgumentId.value && graph.isConnectedTarget(potentialSelfArgumentId.value) ?
    potentialSelfArgumentId.value
  : undefined,
)

onUnmounted(() => graph.unregisterNodeRect(nodeId.value))

const rootNode = ref<HTMLElement>()
const contentNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)

function inputExternalIds() {
  const externalIds = new Array<ExternalId>()
  for (const inputId of graph.db.nodeDependents.reverseLookup(nodeId.value)) {
    const externalId = graph.db.idToExternal(inputId)
    if (externalId) {
      externalIds.push(externalId)
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
  const externalId = graph.db.idToExternal(nodeId.value)
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
      return { type: 'error', text, alwaysShow } satisfies Message
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
const selectionVisible = ref(false)

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
function updateSelectionHover(event: PointerEvent | undefined) {
  selectionHoverPos.value = event && eventScenePos(event)
}

let menuCloseTimeout = ref<ReturnType<typeof setTimeout>>()
const menuEnabledByHover = ref(false)
watchEffect(() => {
  if (menuCloseTimeout.value != null) {
    clearTimeout(menuCloseTimeout.value)
    menuCloseTimeout.value = undefined
  }
  const inZone = (pos: Vec2 | undefined) =>
    pos != null &&
    pos.sub(props.node.position).x < CONTENT_PADDING + ICON_WIDTH + GRAB_HANDLE_X_MARGIN * 2
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

function openFullMenu() {
  menuFull.value = true
  nodeSelection?.setSelection(new Set([nodeId.value]))
}

const isDocsVisible = ref(false)
const visualizationWidth = computed(() => props.node.vis?.width ?? null)
const visualizationHeight = computed(() => props.node.vis?.height ?? null)
const isVisualizationVisible = computed(() => props.node.vis?.visible ?? false)
const isVisualizationFullscreen = computed(() => props.node.vis?.fullscreen ?? false)

watchEffect(() => {
  const size = nodeSize.value
  if (!size.isZero()) {
    emit('update:rect', new Rect(props.node.position, nodeSize.value))
  }
})

const bgStyleVariables = computed(() => {
  const { x: width, y: height } = nodeSize.value
  return {
    '--node-width': `${width}px`,
    '--node-height': `${height}px`,
  }
})

const transform = computed(() => {
  const { x, y } = props.node.position
  return `translate(${x}px, ${y}px)`
})

const startEpochMs = ref(0)
let significantMove = false

const dragPointer = usePointer(
  (pos, event, type) => {
    if (type !== 'start') {
      if (
        !significantMove &&
        (Number(new Date()) - startEpochMs.value >= MAXIMUM_CLICK_LENGTH_MS ||
          pos.relative.lengthSquared() >= MAXIMUM_CLICK_DISTANCE_SQ)
      ) {
        // If this is clearly a drag (not a click), the node itself capture pointer events to
        // prevent `click` on widgets.
        if (event.currentTarget instanceof Element)
          event.currentTarget.setPointerCapture?.(event.pointerId)
        significantMove = true
      }
      const fullOffset = pos.relative
      emit('dragging', fullOffset)
    }
    switch (type) {
      case 'start':
        startEpochMs.value = Number(new Date())
        significantMove = false
        break
      case 'stop': {
        startEpochMs.value = 0
        emit('draggingCommited')
      }
    }
  },
  // Pointer is captured by `target`, to make it receive the `up` and `click` event in case this
  // is not going to be a node drag.
  { pointerCapturedBy: 'target' },
)

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
const outputPortLabel = computed(() => expressionInfo.value?.typename ?? 'Unknown')
const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
const suggestionEntry = computed(() => graph.db.nodeMainSuggestion.lookup(nodeId.value))
const color = computed(() => graph.db.getNodeColorStyle(nodeId.value))
const icon = computed(() => {
  return displayedIconOf(
    suggestionEntry.value,
    expressionInfo.value?.methodCall?.methodPointer,
    outputPortLabel.value,
  )
})

const nodeEditHandler = nodeEditBindings.handler({
  cancel(e) {
    if (e.target instanceof HTMLElement) {
      e.target.blur()
    }
  },
  edit(e) {
    const pos = 'clientX' in e ? new Vec2(e.clientX, e.clientY) : undefined
    startEditingNode(pos)
  },
})

function startEditingNode(position: Vec2 | undefined) {
  let sourceOffset = props.node.rootExpr.code().length
  if (position != null) {
    let domNode, domOffset
    if ((document as any).caretPositionFromPoint) {
      const caret = document.caretPositionFromPoint(position.x, position.y)
      domNode = caret?.offsetNode
      domOffset = caret?.offset
    } else if (document.caretRangeFromPoint) {
      const caret = document.caretRangeFromPoint(position.x, position.y)
      domNode = caret?.startContainer
      domOffset = caret?.startOffset
    } else {
      console.error(
        'Neither `caretPositionFromPoint` nor `caretRangeFromPoint` are supported by this browser',
      )
    }
    if (domNode != null && domOffset != null) {
      sourceOffset = getRelatedSpanOffset(domNode, domOffset)
    }
  }
  emit('update:edited', sourceOffset)
}

function getRelatedSpanOffset(domNode: globalThis.Node, domOffset: number): number {
  if (domNode instanceof HTMLElement && domOffset == 1) {
    const offsetData = domNode.dataset.spanStart
    const offset = (offsetData != null && parseInt(offsetData)) || 0
    const length = domNode.textContent?.length ?? 0
    return offset + length
  } else if (domNode instanceof Text) {
    const siblingEl = domNode.previousElementSibling
    if (siblingEl instanceof HTMLElement) {
      const offsetData = siblingEl.dataset.spanStart
      if (offsetData != null)
        return parseInt(offsetData) + domOffset + (siblingEl.textContent?.length ?? 0)
    }
    const offsetData = domNode.parentElement?.dataset.spanStart
    if (offsetData != null) return parseInt(offsetData) + domOffset
  }
  return domOffset
}

const handlePortClick = useDoubleClick(
  (event: PointerEvent, portId: AstId) => emit('outputPortClick', event, portId),
  (event: PointerEvent, portId: AstId) => emit('outputPortDoubleClick', event, portId),
).handleClick

const handleNodeClick = useDoubleClick(
  (e: MouseEvent) => {
    if (!significantMove) {
      nodeSelection?.handleSelectionOf(e, new Set([nodeId.value]))
      nodeEditHandler(e)
    }
  },
  () => {
    if (!significantMove) emit('doubleClick')
  },
).handleClick

interface PortData {
  clipRange: [number, number]
  label: string | undefined
  portId: AstId
}

const outputPorts = computed((): PortData[] => {
  const ports = outputPortsSet.value
  const numPorts = ports.size
  return Array.from(ports, (portId, index): PortData => {
    return {
      clipRange: [index / numPorts, (index + 1) / numPorts],
      label: numPorts > 1 ? graph.db.getOutputPortIdentifier(portId) : undefined,
      portId,
    }
  })
})

const outputHovered = ref<AstId>()
const hoverAnimations = new Map<AstId, [ReturnType<typeof useApproach>, EffectScope]>()
watchEffect(() => {
  const ports = outputPortsSet.value
  for (const key of hoverAnimations.keys())
    if (!ports.has(key)) {
      hoverAnimations.get(key)?.[1].stop()
      hoverAnimations.delete(key)
    }
  for (const port of outputPortsSet.value) {
    setIfUndefined(hoverAnimations, port, () => {
      // Because `useApproach` uses `onScopeDispose` and we are calling it dynamically (i.e. not at
      // the setup top-level), we need to create a detached scope for each invocation.
      const scope = effectScope(true)
      const approach = scope.run(() =>
        useApproach(
          () =>
            (
              outputHovered.value === port ||
              graph.unconnectedEdge?.target === port ||
              selectionVisible.value
            ) ?
              1
            : 0,
          50,
          0.01,
        ),
      )!
      return [approach, scope]
    })
  }
})

// Clean up dynamically created detached scopes.
onScopeDispose(() => hoverAnimations.forEach(([_, scope]) => scope.stop()))

function portGroupStyle(port: PortData) {
  const [start, end] = port.clipRange
  return {
    '--hover-animation': hoverAnimations.get(port.portId)?.[0].value ?? 0,
    '--port-clip-start': start,
    '--port-clip-end': end,
  }
}

const editingComment = ref(false)

const { getNodeColor, visibleNodeColors } = injectNodeColors()
</script>

<template>
  <div
    ref="rootNode"
    class="GraphNode"
    :style="{
      transform,
      minWidth: isVisualizationVisible ? `${visualizationWidth}px` : undefined,
      '--node-group-color': color,
      ...(node.zIndex ? { 'z-index': node.zIndex } : {}),
    }"
    :class="{
      edited: props.edited,
      selected,
      selectionVisible,
      visualizationVisible: isVisualizationVisible,
      ['executionState-' + executionState]: true,
    }"
    :data-node-id="nodeId"
    @pointerenter="(nodeHovered = true), updateNodeHover($event)"
    @pointerleave="(nodeHovered = false), updateNodeHover(undefined)"
    @pointermove="updateNodeHover"
  >
    <Teleport :to="graphNodeSelections">
      <GraphNodeSelection
        v-if="navigator"
        :nodePosition="props.node.position"
        :nodeSize="nodeSize"
        :selected
        :nodeId
        :color
        @visible="selectionVisible = $event"
        @pointerenter="updateSelectionHover"
        @pointermove="updateSelectionHover"
        @pointerleave="updateSelectionHover(undefined)"
        v-on="dragPointer.events"
        @click="handleNodeClick"
      />
    </Teleport>
    <div class="binding" v-text="node.pattern?.code()" />
    <button
      v-if="!menuVisible && isRecordingOverridden"
      class="overrideRecordButton"
      data-testid="recordingOverriddenButton"
      @click="isRecordingOverridden = false"
    >
      <SvgIcon name="record" />
    </button>
    <CircularMenu
      v-if="menuVisible"
      v-model:isRecordingOverridden="isRecordingOverridden"
      v-model:isDocsVisible="isDocsVisible"
      :isRecordingEnabledGlobally="projectStore.isRecordingEnabled"
      :isVisualizationVisible="isVisualizationVisible"
      :isFullMenuVisible="menuVisible && menuFull"
      :nodeColor="getNodeColor(nodeId)"
      :visibleNodeColors="visibleNodeColors"
      @update:isVisualizationVisible="emit('update:visualizationVisible', $event)"
      @startEditing="startEditingNode"
      @startEditingComment="editingComment = true"
      @openFullMenu="openFullMenu"
      @delete="emit('delete')"
      @createNodes="emit('createNodes', $event)"
      @pointerenter="menuHovered = true"
      @pointerleave="menuHovered = false"
      @update:nodeColor="emit('setNodeColor', $event)"
    />
    <GraphVisualization
      v-if="isVisualizationVisible"
      :nodeSize="nodeSize"
      :scale="navigator?.scale ?? 1"
      :nodePosition="props.node.position"
      :isCircularMenuVisible="menuVisible"
      :currentType="props.node.vis?.identifier"
      :isFullscreen="isVisualizationFullscreen"
      :dataSource="{ type: 'node', nodeId: props.node.rootExpr.externalId }"
      :typename="expressionInfo?.typename"
      :width="visualizationWidth"
      :height="visualizationHeight"
      :isFocused="isOnlyOneSelected"
      @update:rect="emit('update:visualizationRect', $event)"
      @update:id="emit('update:visualizationId', $event)"
      @update:visible="emit('update:visualizationVisible', $event)"
      @update:fullscreen="emit('update:visualizationFullscreen', $event)"
      @update:width="emit('update:visualizationWidth', $event)"
      @update:height="emit('update:visualizationHeight', $event)"
      @update:nodePosition="graph.setNodePosition(nodeId, $event)"
      @createNodes="emit('createNodes', $event)"
    />
    <GraphNodeComment v-model:editing="editingComment" :node="node" class="beforeNode" />
    <div ref="contentNode" class="content" v-on="dragPointer.events" @click="handleNodeClick">
      <NodeWidgetTree
        :ast="props.node.innerExpr"
        :nodeId="nodeId"
        :nodeElement="rootNode"
        :nodeSize="nodeSize"
        :icon="icon"
        :connectedSelfArgumentId="connectedSelfArgumentId"
        :potentialSelfArgumentId="potentialSelfArgumentId"
        :conditionalPorts="props.node.conditionalPorts"
        :extended="isOnlyOneSelected"
        @openFullMenu="openFullMenu"
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
      class="afterNode"
      :class="{ messageWithMenu: menuVisible }"
      :message="visibleMessage.text"
      :type="visibleMessage.type"
    />
    <svg class="bgPaths" :style="bgStyleVariables">
      <rect class="bgFill" />
      <template v-for="port of outputPorts" :key="port.portId">
        <g :style="portGroupStyle(port)">
          <g class="portClip">
            <rect
              class="outputPortHoverArea"
              @pointerenter="outputHovered = port.portId"
              @pointerleave="outputHovered = undefined"
              @pointerdown.stop.prevent="handlePortClick($event, port.portId)"
            />
            <rect class="outputPort" />
          </g>
          <text class="outputPortLabel">{{ port.label }}</text>
        </g>
      </template>
    </svg>
  </div>
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

  --output-port-max-width: 4px;
  --output-port-overlap: -8px;
  --output-port-hover-width: 20px;
}

.outputPort,
.outputPortHoverArea {
  x: calc(0px - var(--output-port-width) / 2);
  width: calc(var(--node-width) + var(--output-port-width));
  rx: calc(var(--node-border-radius) + var(--output-port-width) / 2);

  fill: none;
  stroke: var(--node-color-port);
  stroke-width: calc(var(--output-port-width) + var(--output-port-overlap));
  transition: stroke 0.2s ease;
  --horizontal-line: calc(var(--node-width) - var(--node-border-radius) * 2);
  --vertical-line: calc(var(--node-height) - var(--node-border-radius) * 2);
  --radius-arclength: calc(
    (var(--node-border-radius) + var(--output-port-width) * 0.5) * 2 * 3.141592653589793
  );

  stroke-dasharray: calc(var(--horizontal-line) + var(--radius-arclength) * 0.5) 10000%;
  stroke-dashoffset: calc(
    0px - var(--horizontal-line) - var(--vertical-line) - var(--radius-arclength) * 0.25
  );
  stroke-linecap: round;
}

.outputPort {
  --output-port-width: calc(
    var(--output-port-max-width) * var(--hover-animation) - var(--output-port-overlap)
  );
  y: calc(0px - var(--output-port-width) / 2);
  height: calc(var(--node-height) + var(--output-port-width));
  pointer-events: none;
}

.outputPortHoverArea {
  --output-port-width: var(--output-port-hover-width);
  y: calc(
    0px + var(--output-port-hover-width) / 2 + var(--output-port-overlap) / 2 + var(--node-height) /
      2
  );
  height: calc(var(--node-height) / 2 + var(--output-port-hover-width) / 2);
  stroke: transparent;
  pointer-events: all;
  cursor: pointer;
}

.portClip {
  clip-path: inset(
    0 calc((1 - var(--port-clip-end)) * (100% + 1px) - 0.5px) 0
      calc(var(--port-clip-start) * (100% + 1px) + 0.5px)
  );
}

.outputPortLabel {
  user-select: none;
  pointer-events: none;
  z-index: 10;
  text-anchor: middle;
  opacity: calc(var(--hover-animation) * var(--hover-animation));
  fill: var(--node-color-primary);
  transform: translate(50%, calc(var(--node-height) + var(--output-port-max-width) + 16px));
}

.bgFill {
  width: var(--node-width);
  height: var(--node-height);
  rx: var(--node-border-radius);

  fill: var(--node-color-primary);
  transition: fill 0.2s ease;
}

.GraphNode {
  --node-height: 32px;

  --node-color-primary: color-mix(
    in oklab,
    var(--node-group-color) 100%,
    var(--node-group-color) 0%
  );
  --node-color-port: color-mix(in oklab, var(--node-color-primary) 85%, white 15%);
  --node-color-error: color-mix(in oklab, var(--node-group-color) 30%, rgb(255, 0, 0) 70%);

  &.executionState-Unknown,
  &.executionState-Pending {
    --node-color-primary: color-mix(in oklab, var(--node-group-color) 60%, #aaa 40%);
  }

  position: absolute;
  border-radius: var(--node-border-radius);
  transition: box-shadow 0.2s ease-in-out;
  box-sizing: border-box;
  ::selection {
    background-color: rgba(255, 255, 255, 20%);
  }
}

.GraphNode.edited {
  display: none;
}

.content {
  font-family: var(--font-code);
  position: relative;
  top: 0;
  left: 0;
  caret-shape: bar;
  height: var(--node-height);
  border-radius: var(--node-border-radius);
  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  padding: v-bind('CONTENT_PADDING_PX');
  padding-right: v-bind('CONTENT_PADDING_RIGHT_PX');
  z-index: 2;
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

.selectionVisible .binding {
  opacity: 1;
}

.container {
  position: relative;
  display: flex;
  gap: 4px;
}

.CircularMenu {
  z-index: 25;
}

.CircularMenu.partial {
  z-index: 1;
}

.beforeNode {
  position: absolute;
  bottom: 100%;
  left: 60px;
  width: calc(max(100% - 60px, 800px));
  margin-bottom: 2px;
}

.afterNode {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}

.messageWithMenu {
  left: 40px;
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

.GraphNode.selectionVisible .statuses {
  opacity: 0;
}

.overrideRecordButton {
  position: absolute;
  cursor: pointer;
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
</style>
