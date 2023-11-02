<script setup lang="ts">
import { nodeEditBindings } from '@/bindings'
import CircularMenu from '@/components/CircularMenu.vue'
import NodeTree from '@/components/GraphEditor/NodeTree.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectGraphSelection } from '@/providers/graphSelection'
import type { Node } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { useApproach } from '@/util/animation'
import { colorFromString } from '@/util/colors'
import { usePointer, useResizeObserver } from '@/util/events'
import { methodNameToIcon, typeNameToIcon } from '@/util/getIconName'
import type { Opt } from '@/util/opt'
import { qnJoin, tryQualifiedName } from '@/util/qualifiedName'
import { Rect } from '@/util/rect'
import { unwrap } from '@/util/result'
import { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId, VisualizationIdentifier } from 'shared/yjsModel'
import { computed, onUpdated, reactive, ref, watch, watchEffect } from 'vue'
import GraphVisualization from './GraphVisualization.vue'

const MAXIMUM_CLICK_LENGTH_MS = 300
const MAXIMUM_CLICK_DISTANCE_SQ = 50

const props = defineProps<{
  node: Node
}>()

const emit = defineEmits<{
  updateRect: [rect: Rect]
  updateExprRect: [id: ExprId, rect: Rect]
  updateContent: [updates: [range: ContentRange, content: string][]]
  movePosition: [delta: Vec2]
  setVisualizationId: [id: Opt<VisualizationIdentifier>]
  setVisualizationVisible: [visible: boolean]
  delete: []
  replaceSelection: []
  'update:selected': [selected: boolean]
  outputPortAction: []
}>()

const nodeSelection = injectGraphSelection(true)

const nodeId = computed(() => props.node.rootSpan.astId)
const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)
const editableRootNode = ref<HTMLElement>()
const menuVisible = ref(false)

const isSelected = computed(() => nodeSelection?.isSelected(nodeId.value) ?? false)
watch(isSelected, (selected) => {
  menuVisible.value = menuVisible.value && selected
})

const isAutoEvaluationDisabled = ref(false)
const isDocsVisible = ref(false)
const isVisualizationVisible = computed(() => props.node.vis?.visible ?? false)

const projectStore = useProjectStore()

watchEffect(() => {
  const size = nodeSize.value
  if (!size.isZero()) {
    emit('updateRect', new Rect(props.node.position, nodeSize.value))
  }
})

const outputHovered = ref(false)
const hoverAnimation = useApproach(() => (outputHovered.value ? 1 : 0), 50, 0.01)

const bgStyleVariables = computed(() => {
  return {
    '--node-width': `${nodeSize.value.x}px`,
    '--node-height': `${nodeSize.value.y}px`,
    '--hover-animation': `${hoverAnimation.value}`,
  }
})

const transform = computed(() => {
  let pos = props.node.position
  return `translate(${pos.x}px, ${pos.y}px)`
})

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
  return 0
}

function updateRange(range: ContentRange, threhsold: number, adjust: number) {
  range[0] = updateOffset(range[0], threhsold, adjust)
  range[1] = updateOffset(range[1], threhsold, adjust)
}

function updateOffset(offset: number, threhsold: number, adjust: number) {
  return offset >= threhsold ? offset + adjust : offset
}

function updateExprRect(id: ExprId, rect: Rect) {
  emit('updateExprRect', id, rect)
}

interface TextEdit {
  range: ContentRange
  content: string
}

const editsToApply = reactive<TextEdit[]>([])

function editContent(e: Event) {
  e.preventDefault()
  if (!(e instanceof InputEvent)) return

  const domRanges = e.getTargetRanges()
  const ranges = domRanges.map<ContentRange>((r) => {
    return [
      getRelatedSpanOffset(r.startContainer, r.startOffset),
      getRelatedSpanOffset(r.endContainer, r.endOffset),
    ]
  })

  switch (e.inputType) {
    case 'insertText': {
      const content = e.data ?? ''
      for (let range of ranges) {
        if (range[0] != range[1]) {
          editsToApply.push({ range, content: '' })
        }
        editsToApply.push({ range: [range[1], range[1]], content })
      }
      break
    }
    case 'insertFromDrop':
    case 'insertFromPaste': {
      const content = e.dataTransfer?.getData('text/plain')
      if (content != null) {
        for (let range of ranges) {
          editsToApply.push({ range, content })
        }
      }
      break
    }
    case 'deleteByCut':
    case 'deleteWordBackward':
    case 'deleteWordForward':
    case 'deleteContentBackward':
    case 'deleteContentForward':
    case 'deleteByDrag': {
      for (let range of ranges) {
        editsToApply.push({ range, content: '' })
      }
      break
    }
  }
}

watch(editsToApply, () => {
  if (editsToApply.length === 0) return
  saveSelections()
  let edit: TextEdit | undefined
  const updates: [ContentRange, string][] = []
  while ((edit = editsToApply.shift())) {
    const range = edit.range
    const content = edit.content
    const adjust = content.length - (range[1] - range[0])
    editsToApply.forEach((e) => updateRange(e.range, range[1], adjust))
    if (selectionToRecover) {
      selectionToRecover.ranges.forEach((r) => updateRange(r, range[1], adjust))
      if (selectionToRecover.anchor != null) {
        selectionToRecover.anchor = updateOffset(selectionToRecover.anchor, range[1], adjust)
      }
      if (selectionToRecover.focus != null) {
        selectionToRecover.focus = updateOffset(selectionToRecover.focus, range[1], adjust)
      }
    }
    updates.push([range, content])
  }
  emit('updateContent', updates)
})

interface SavedSelections {
  anchor: number | null
  focus: number | null
  ranges: ContentRange[]
}

let selectionToRecover: SavedSelections | null = null

function saveSelections() {
  const root = editableRootNode.value
  const selection = window.getSelection()
  if (root == null || selection == null || !selection.containsNode(root, true)) return
  const ranges: ContentRange[] = Array.from({ length: selection.rangeCount }, (_, i) =>
    selection.getRangeAt(i),
  )
    .filter((r) => r.intersectsNode(root))
    .map((r) => [
      getRelatedSpanOffset(r.startContainer, r.startOffset),
      getRelatedSpanOffset(r.endContainer, r.endOffset),
    ])

  let anchor =
    selection.anchorNode && root.contains(selection.anchorNode)
      ? getRelatedSpanOffset(selection.anchorNode, selection.anchorOffset)
      : null
  let focus =
    selection.focusNode && root.contains(selection.focusNode)
      ? getRelatedSpanOffset(selection.focusNode, selection.focusOffset)
      : null

  selectionToRecover = {
    anchor,
    focus,
    ranges,
  }
}

onUpdated(() => {
  const root = editableRootNode.value

  function findTextNodeAtOffset(offset: number | null): { node: Text; offset: number } | null {
    if (offset == null) return null
    for (let textSpan of root?.querySelectorAll<HTMLSpanElement>('span[data-span-start]') ?? []) {
      if (textSpan.children.length > 0) continue
      const start = parseInt(textSpan.dataset.spanStart ?? '0')
      const text = textSpan.textContent ?? ''
      const end = start + text.length
      if (start <= offset && offset <= end) {
        let remainingOffset = offset - start
        for (let node of textSpan.childNodes) {
          if (node instanceof Text) {
            let length = node.data.length
            if (remainingOffset > length) {
              remainingOffset -= length
            } else {
              return {
                node,
                offset: remainingOffset,
              }
            }
          }
        }
      }
    }
    return null
  }

  if (selectionToRecover != null && editableRootNode.value != null) {
    const saved = selectionToRecover
    selectionToRecover = null
    const selection = window.getSelection()
    if (selection == null) return

    for (let range of saved.ranges) {
      const start = findTextNodeAtOffset(range[0])
      const end = findTextNodeAtOffset(range[1])
      if (start == null || end == null) continue
      let newRange = document.createRange()
      newRange.setStart(start.node, start.offset)
      newRange.setEnd(end.node, end.offset)
      selection.addRange(newRange)
    }
    if (saved.anchor != null || saved.focus != null) {
      const anchor = findTextNodeAtOffset(saved.anchor) ?? {
        node: selection.anchorNode,
        offset: selection.anchorOffset,
      }
      const focus = findTextNodeAtOffset(saved.focus) ?? {
        node: selection.focusNode,
        offset: selection.focusOffset,
      }
      if (anchor.node == null || focus.node == null) return
      selection.setBaseAndExtent(anchor.node, anchor.offset, focus.node, focus.offset)
    }
  }
})
watch(
  () => [isAutoEvaluationDisabled.value, isDocsVisible.value, isVisualizationVisible.value],
  () => {
    rootNode.value?.focus()
  },
)

const editableKeydownHandler = nodeEditBindings.handler({
  selectAll() {
    const element = editableRootNode.value
    const selection = window.getSelection()
    if (element == null || selection == null) return
    const range = document.createRange()
    range.selectNodeContents(element)
    selection.removeAllRanges()
    selection.addRange(range)
  },
})

const startEpochMs = ref(0)
let startEvent: PointerEvent | null = null
let startPos = Vec2.Zero

const dragPointer = usePointer((pos, event, type) => {
  emit('movePosition', pos.delta)
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
    }
  }
})

const suggestionDbStore = useSuggestionDbStore()

const expressionInfo = computed(() =>
  projectStore.computedValueRegistry.getExpressionInfo(props.node.rootSpan.astId),
)
const outputTypeName = computed(() => expressionInfo.value?.typename ?? 'Unknown')
const executionState = computed(() => expressionInfo.value?.payload.type ?? 'Unknown')
const suggestionEntry = computed(() => {
  const method = expressionInfo.value?.methodCall?.methodPointer
  if (method == null) return undefined
  const typeName = tryQualifiedName(method.definedOnType)
  const methodName = tryQualifiedName(method.name)
  if (!typeName.ok || !methodName.ok) return undefined
  const qualifiedName = qnJoin(unwrap(typeName), unwrap(methodName))
  const [id] = suggestionDbStore.entries.nameToId.lookup(qualifiedName)
  if (id == null) return undefined
  return suggestionDbStore.entries.get(id)
})
const icon = computed(() => {
  if (suggestionEntry.value?.iconName) {
    return suggestionEntry.value.iconName
  }
  const methodName = expressionInfo.value?.methodCall?.methodPointer.name
  if (methodName != null) {
    return methodNameToIcon(methodName)
  } else if (outputTypeName.value != null) {
    return typeNameToIcon(outputTypeName.value)
  } else {
    return 'in_out'
  }
})
const color = computed(() =>
  suggestionEntry.value?.groupIndex != null
    ? `var(--group-color-${suggestionDbStore.groups[suggestionEntry.value.groupIndex]?.name})`
    : colorFromString(expressionInfo.value?.typename ?? 'Unknown'),
)

function hoverExpr(id: ExprId | undefined) {
  if (nodeSelection != null) nodeSelection.hoveredExpr = id
}
</script>

<template>
  <div
    ref="rootNode"
    class="GraphNode"
    :style="{
      transform,
      '--node-group-color': color,
    }"
    :class="{
      dragging: dragPointer.dragging,
      selected: nodeSelection?.isSelected(nodeId),
      visualizationVisible: isVisualizationVisible,
      ['executionState-' + executionState]: true,
    }"
  >
    <div class="selection" v-on="dragPointer.events"></div>
    <div class="binding" @pointerdown.stop>
      {{ node.binding }}
    </div>
    <CircularMenu
      v-if="menuVisible"
      v-model:isAutoEvaluationDisabled="isAutoEvaluationDisabled"
      v-model:isDocsVisible="isDocsVisible"
      :isVisualizationVisible="isVisualizationVisible"
      @update:isVisualizationVisible="emit('setVisualizationVisible', $event)"
    />
    <GraphVisualization
      v-if="isVisualizationVisible"
      :nodeSize="nodeSize"
      :isCircularMenuVisible="menuVisible"
      :currentType="props.node.vis"
      :expressionId="props.node.rootSpan.astId"
      :typename="expressionInfo?.typename"
      @setVisualizationId="emit('setVisualizationId', $event)"
      @setVisualizationVisible="emit('setVisualizationVisible', $event)"
    />
    <div class="node" v-on="dragPointer.events">
      <SvgIcon class="icon grab-handle" :name="icon"></SvgIcon
      ><span
        ref="editableRootNode"
        class="editable"
        contenteditable
        spellcheck="false"
        @beforeinput="editContent"
        @keydown="editableKeydownHandler"
        @pointerdown.stop
        @blur="projectStore.stopCapturingUndo()"
        ><NodeTree
          :ast="node.rootSpan"
          :nodeSpanStart="node.rootSpan.span()[0]"
          @updateExprRect="updateExprRect"
          @updateHoveredExpr="hoverExpr($event)"
      /></span>
    </div>
    <svg class="bgPaths" :style="bgStyleVariables">
      <rect class="bgFill" />
      <rect
        class="outputPortHoverArea"
        @pointerenter="outputHovered = true"
        @pointerleave="outputHovered = false"
        @pointerdown="emit('outputPortAction')"
      />
      <rect class="outputPort" />
    </svg>
    <div class="outputTypeName">{{ outputTypeName }}</div>
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
  --output-port-overlap: 0.1px;
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

.bgFill {
  width: var(--node-width);
  height: var(--node-height);
  rx: var(--node-border-radius);

  fill: var(--node-color-primary);
  transition: fill 0.2s ease;
}

.bgPaths .bgPaths:hover {
  opacity: 1;
}

.GraphNode {
  --node-height: 32px;
  --node-border-radius: 16px;

  --node-group-color: #357ab9;

  --node-color-primary: color-mix(
    in oklab,
    var(--node-group-color) 100%,
    var(--node-group-color) 0%
  );
  --node-color-port: color-mix(in oklab, var(--node-color-primary) 75%, white 15%);
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

.node {
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
  padding: 4px 8px;
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
  user-select: none;
  margin-right: 10px;
  color: black;
  position: absolute;
  right: 100%;
  top: 50%;
  transform: translateY(-50%);
  opacity: 0;
  transition: opacity 0.2s ease-in-out;
}

.GraphNode .selection:hover + .binding,
.GraphNode.selected .binding {
  opacity: 1;
}

.editable {
  outline: none;
  height: 24px;
  display: inline-flex;
  align-items: center;
}

.container {
  position: relative;
  display: flex;
  gap: 4px;
}

.grab-handle {
  color: white;
  margin-right: 10px;
}

.CircularMenu {
  z-index: 1;
}

.outputTypeName {
  user-select: none;
  position: absolute;
  left: 50%;
  top: 110%;
  transform: translateX(-50%);
  opacity: 0;
  transition: opacity 0.3s ease-in-out;
  pointer-events: none;
  z-index: 10;
  color: var(--node-color-primary);
}

.GraphNode:hover .outputTypeName {
  opacity: 1;
}
</style>
