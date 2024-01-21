<script lang="ts">
import { nodeEditBindings } from '@/bindings'
import CircularMenu from '@/components/CircularMenu.vue'
import GraphNodeError from '@/components/GraphEditor/GraphNodeError.vue'
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import NodeWidgetTree from '@/components/GraphEditor/NodeWidgetTree.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useApproach } from '@/composables/animation'
import { useDoubleClick } from '@/composables/doubleClick'
import { usePointer, useResizeObserver } from '@/composables/events'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore, type Node } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { Ast } from '@/util/ast'
import { Prefixes } from '@/util/ast/prefixes'
import type { Opt } from '@/util/data/opt'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { displayedIconOf } from '@/util/getIconName'
import { setIfUndefined } from 'lib0/map'
import type { ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onUnmounted, ref, watch, watchEffect } from 'vue'

const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50
/** The width in pixels that is not the widget tree. This includes the icon, and padding. */
const NODE_EXTRA_WIDTH_PX = 30

const prefixes = Prefixes.FromLines({
  enableOutputContext:
    'Standard.Base.Runtime.with_enabled_context Standard.Base.Runtime.Context.Output __ <| __',
  disableOutputContext:
    'Standard.Base.Runtime.with_disabled_context Standard.Base.Runtime.Context.Output __ <| __',
  // Currently unused; included as PoC.
  skip: 'SKIP __',
  freeze: 'FREEZE __',
})
</script>

<script setup lang="ts">
const props = defineProps<{
  node: Node
  edited: boolean
}>()

const emit = defineEmits<{
  dragging: [offset: Vec2]
  draggingCommited: []
  delete: []
  replaceSelection: []
  outputPortClick: [portId: ExprId]
  outputPortDoubleClick: [portId: ExprId]
  doubleClick: []
  'update:edited': [cursorPosition: number]
  'update:rect': [rect: Rect]
  'update:visualizationId': [id: Opt<VisualizationIdentifier>]
  'update:visualizationRect': [rect: Rect | undefined]
  'update:visualizationVisible': [visible: boolean]
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

const widthOverridePx = ref<number>()
const nodeId = computed(() => props.node.rootSpan.exprId)

onUnmounted(() => graph.unregisterNodeRect(nodeId.value))

const rootNode = ref<HTMLElement>()
const contentNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)
const baseNodeSize = computed(
  () => new Vec2((contentNode.value?.scrollWidth ?? 0) + NODE_EXTRA_WIDTH_PX, nodeSize.value.y),
)
const menuVisible = ref(false)

const error = computed(() => {
  const info = projectStore.computedValueRegistry.db.get(nodeId.value)
  switch (info?.payload.type) {
    case 'Panic': {
      return info.payload.message
    }
    case 'DataflowError': {
      return projectStore.dataflowErrors.lookup(nodeId.value)?.value?.message.split(' (at')[0]
    }
    default:
      return undefined
  }
})

const isSelected = computed(() => nodeSelection?.isSelected(nodeId.value) ?? false)
watch(isSelected, (selected) => {
  menuVisible.value = menuVisible.value && selected
})

const isDocsVisible = ref(false)
const isVisualizationVisible = computed(() => props.node.vis?.visible ?? false)

watchEffect(() => {
  const size = nodeSize.value
  if (!size.isZero()) {
    emit('update:rect', new Rect(props.node.position, nodeSize.value))
  }
})

const bgStyleVariables = computed(() => {
  return {
    '--node-width': `${nodeSize.value.x}px`,
    '--node-height': `${nodeSize.value.y}px`,
  }
})

const transform = computed(() => {
  let pos = props.node.position
  return `translate(${pos.x}px, ${pos.y}px)`
})

const startEpochMs = ref(0)
let startEvent: PointerEvent | null = null
let startPos = Vec2.Zero

const dragPointer = usePointer((pos, event, type) => {
  if (type !== 'start') {
    const fullOffset = pos.absolute.sub(startPos)
    emit('dragging', fullOffset)
  }
  switch (type) {
    case 'start': {
      startEpochMs.value = Number(new Date())
      startEvent = event
      startPos = pos.absolute
      event.stopImmediatePropagation()
      break
    }
    case 'stop': {
      if (
        Number(new Date()) - startEpochMs.value <= MAXIMUM_CLICK_LENGTH_MS &&
        startEvent != null &&
        pos.absolute.distanceSquared(startPos) <= MAXIMUM_CLICK_DISTANCE_SQ
      ) {
        nodeSelection?.handleSelectionOf(startEvent, new Set([nodeId.value]))
        menuVisible.value = true
      }
      startEvent = null
      startEpochMs.value = 0
      emit('draggingCommited')
    }
  }
})

const matches = computed(() => prefixes.extractMatches(props.node.rootSpan))
const displayedExpression = computed(() => matches.value.innerExpr)

const isOutputContextOverridden = computed({
  get() {
    const override =
      matches.value.matches.enableOutputContext ?? matches.value.matches.disableOutputContext
    const overrideEnabled = matches.value.matches.enableOutputContext != null
    // An override is only counted as enabled if it is currently in effect. This requires:
    // - that an override exists
    if (!override) return false
    // - that it is setting the "enabled" value to a non-default value
    else if (overrideEnabled === projectStore.isOutputContextEnabled) return false
    // - and that it applies to the current execution context.
    else {
      const contextWithoutQuotes = override[0]?.code().replace(/^['"]|['"]$/g, '')
      return contextWithoutQuotes === projectStore.executionMode
    }
  },
  set(shouldOverride) {
    const module = projectStore.module
    if (!module) return
    const edit = props.node.rootSpan.module.edit()
    const replacementText = shouldOverride
      ? [Ast.TextLiteral.new(projectStore.executionMode, edit)]
      : undefined
    const replacements = projectStore.isOutputContextEnabled
      ? {
          enableOutputContext: undefined,
          disableOutputContext: replacementText,
        }
      : {
          enableOutputContext: replacementText,
          disableOutputContext: undefined,
        }
    const expression = props.node.rootSpan
    const newAst = prefixes.modify(edit, expression, replacements)
    const code = newAst.code()
    graph.setNodeContent(props.node.rootSpan.exprId, code)
  },
})

// FIXME [sb]: https://github.com/enso-org/enso/issues/8442
// This does not take into account `displayedExpression`.
const expressionInfo = computed(() => graph.db.getExpressionInfo(nodeId.value))
const outputPortLabel = computed(() => expressionInfo.value?.typename ?? 'Unknown')
const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
const suggestionEntry = computed(() => graph.db.nodeMainSuggestion.lookup(nodeId.value))
const color = computed(() => graph.db.getNodeColorStyle(nodeId.value))
const icon = computed(() => {
  const expressionInfo = graph.db.getExpressionInfo(nodeId.value)
  return displayedIconOf(
    suggestionEntry.value,
    expressionInfo?.methodCall?.methodPointer,
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
  let sourceOffset = 0
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
  (portId: ExprId) => emit('outputPortClick', portId),
  (portId: ExprId) => emit('outputPortDoubleClick', portId),
).handleClick

const handleNodeClick = useDoubleClick(
  (e: MouseEvent) => nodeEditHandler(e),
  () => emit('doubleClick'),
).handleClick

interface PortData {
  clipRange: [number, number]
  label: string
  portId: ExprId
}

const outputPorts = computed((): PortData[] => {
  const ports = outputPortsSet.value
  const numPorts = ports.size
  return Array.from(ports, (portId, index): PortData => {
    const labelIdent = numPorts > 1 ? graph.db.getOutputPortIdentifier(portId) + ': ' : ''
    const labelType =
      graph.db.getExpressionInfo(numPorts > 1 ? portId : nodeId.value)?.typename ?? 'Unknown'
    return {
      clipRange: [index / numPorts, (index + 1) / numPorts],
      label: labelIdent + labelType,
      portId,
    }
  })
})

const outputHovered = ref<ExprId>()
const hoverAnimations = new Map<ExprId, ReturnType<typeof useApproach>>()
watchEffect(() => {
  const ports = outputPortsSet.value
  for (const key of hoverAnimations.keys()) if (!ports.has(key)) hoverAnimations.delete(key)
  for (const port of outputPortsSet.value) {
    setIfUndefined(hoverAnimations, port, () =>
      useApproach(
        () => (outputHovered.value === port || graph.unconnectedEdge?.target === port ? 1 : 0),
        50,
        0.01,
      ),
    )
  }
})

function portGroupStyle(port: PortData) {
  const [start, end] = port.clipRange
  return {
    '--hover-animation': hoverAnimations.get(port.portId)?.value ?? 0,
    '--port-clip-start': start,
    '--port-clip-end': end,
  }
}
</script>

<template>
  <div
    ref="rootNode"
    class="GraphNode"
    :style="{
      transform,
      width:
        widthOverridePx != null && isVisualizationVisible
          ? `${Math.max(widthOverridePx, (contentNode?.scrollWidth ?? 0) + NODE_EXTRA_WIDTH_PX)}px`
          : undefined,
      '--node-group-color': color,
    }"
    :class="{
      edited: props.edited,
      dragging: dragPointer.dragging,
      selected: nodeSelection?.isSelected(nodeId),
      visualizationVisible: isVisualizationVisible,
      ['executionState-' + executionState]: true,
    }"
  >
    <div class="selection" v-on="dragPointer.events"></div>
    <div class="binding" @pointerdown.stop>
      {{ node.pattern?.code() ?? '' }}
    </div>
    <CircularMenu
      v-if="menuVisible"
      v-model:isOutputContextOverridden="isOutputContextOverridden"
      v-model:isDocsVisible="isDocsVisible"
      :isOutputContextEnabledGlobally="projectStore.isOutputContextEnabled"
      :isVisualizationVisible="isVisualizationVisible"
      @update:isVisualizationVisible="emit('update:visualizationVisible', $event)"
    />
    <GraphVisualization
      v-if="isVisualizationVisible"
      :nodeSize="baseNodeSize"
      :scale="navigator?.scale ?? 1"
      :nodePosition="props.node.position"
      :isCircularMenuVisible="menuVisible"
      :currentType="node.vis?.identifier"
      :dataSource="{ type: 'node', nodeId }"
      :typename="expressionInfo?.typename"
      @update:rect="
        emit('update:visualizationRect', $event),
          (widthOverridePx = $event && $event.size.x > baseNodeSize.x ? $event.size.x : undefined)
      "
      @update:id="emit('update:visualizationId', $event)"
      @update:visible="emit('update:visualizationVisible', $event)"
    />
    <div class="node" @pointerdown="handleNodeClick" v-on="dragPointer.events">
      <SvgIcon class="icon grab-handle" :name="icon"></SvgIcon>
      <div ref="contentNode" class="widget-tree">
        <NodeWidgetTree :ast="displayedExpression" />
      </div>
    </div>
    <GraphNodeError v-if="error" class="error" :error="error" />
    <svg class="bgPaths" :style="bgStyleVariables">
      <rect class="bgFill" />
      <template v-for="port of outputPorts" :key="port.portId">
        <g :style="portGroupStyle(port)">
          <g class="portClip">
            <rect
              class="outputPortHoverArea"
              @pointerenter="outputHovered = port.portId"
              @pointerleave="outputHovered = undefined"
              @pointerdown.stop.prevent="handlePortClick(port.portId)"
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
  top: 0px;
  left: 0px;
  display: flex;

  --output-port-max-width: 6px;
  --output-port-overlap: 0.2px;
  --output-port-hover-width: 8px;
}

.outputPort,
.outputPortHoverArea {
  x: calc(0px - var(--output-port-width) / 2);
  y: calc(0px - var(--output-port-width) / 2);
  width: calc(var(--node-width) + var(--output-port-width));
  height: calc(var(--node-height) + var(--output-port-width));
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
  pointer-events: none;
}

.outputPortHoverArea {
  --output-port-width: var(--output-port-hover-width);
  stroke: transparent;
  pointer-events: all;
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
  --node-border-radius: 16px;

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
  ::selection {
    background-color: rgba(255, 255, 255, 20%);
  }
}

.GraphNode.edited {
  display: none;
}

.node {
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
  padding: 4px;
  padding-right: 8px;
  z-index: 2;
  transition: outline 0.2s ease;
  outline: 0px solid transparent;
}

.GraphNode .selection {
  position: absolute;
  inset: calc(0px - var(--selected-node-border-width));
  --node-current-selection-width: 0px;

  &:before {
    content: '';
    opacity: 0;
    position: absolute;
    border-radius: var(--node-border-radius);
    display: block;
    inset: var(--selected-node-border-width);
    box-shadow: 0 0 0 var(--node-current-selection-width) var(--node-color-primary);

    transition:
      box-shadow 0.2s ease-in-out,
      opacity 0.2s ease-in-out;
  }
}

.GraphNode:is(:hover, .selected) .selection:before,
.GraphNode .selection:hover:before {
  --node-current-selection-width: var(--selected-node-border-width);
}

.GraphNode .selection:hover:before {
  opacity: 0.15;
}
.GraphNode.selected .selection:before {
  opacity: 0.2;
}

.GraphNode.selected .selection:hover:before {
  opacity: 0.3;
}

.binding {
  font-family: var(--font-code);
  user-select: none;
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

.GraphNode .selection:hover + .binding,
.GraphNode.selected .binding {
  opacity: 1;
}

.container {
  position: relative;
  display: flex;
  gap: 4px;
}

.grab-handle {
  color: white;
  margin: 0 4px;
}

.CircularMenu {
  z-index: 1;
}

.error {
  position: absolute;
  top: 100%;
  margin-top: 4px;
}
</style>
