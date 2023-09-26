<script setup lang="ts">
import { computed, onUpdated, reactive, ref, shallowRef, watch, watchEffect } from 'vue'

import CircularMenu from '@/components/CircularMenu.vue'
import NodeSpan from '@/components/NodeSpan.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import {
  provideVisualizationConfig,
  type VisualizationConfig,
} from '@/providers/visualizationConfig'
import type { Node } from '@/stores/graph'
import { Rect } from '@/stores/rect'
import { useVisualizationStore, type Visualization } from '@/stores/visualization'
import { useDocumentEvent, usePointer, useResizeObserver } from '@/util/events'
import type { Vec2 } from '@/util/vec2'
import type { ContentRange, ExprId } from 'shared/yjsModel'

const props = defineProps<{
  node: Node
}>()

const emit = defineEmits<{
  updateRect: [rect: Rect]
  updateExprRect: [id: ExprId, rect: Rect]
  updateContent: [range: ContentRange, content: string]
  movePosition: [delta: Vec2]
  delete: []
}>()

const visualizationStore = useVisualizationStore()

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)
const editableRootNode = ref<HTMLElement>()

watchEffect(() => {
  const size = nodeSize.value
  if (!size.isZero()) {
    emit('updateRect', new Rect(props.node.position, nodeSize.value))
  }
})

const dragPointer = usePointer((event) => {
  emit('movePosition', event.delta)
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
    emit('updateContent', range, content)
  }
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

function handleClick(e: PointerEvent) {
  if (e.shiftKey) {
    emit('delete')
    e.preventDefault()
    e.stopPropagation()
  }
}

const isCircularMenuVisible = ref(false)
const isAutoEvaluationDisabled = ref(false)
const isDocsVisible = ref(false)
const isVisualizationVisible = ref(false)

const visualizationType = ref('Scatterplot')
const visualization = shallowRef<Visualization>()

const queuedVisualizationData = computed<{}>(() =>
  visualizationStore.sampleData(visualizationType.value),
)
const visualizationData = ref<{}>({})

function isInputEvent(event: Event): event is Event & { target: HTMLElement } {
  return (
    !(event.target instanceof HTMLElement) ||
    !rootNode.value?.contains(event.target) ||
    event.target.isContentEditable ||
    event.target instanceof HTMLInputElement ||
    event.target instanceof HTMLTextAreaElement
  )
}

const visualizationConfig = ref<VisualizationConfig>({
  fullscreen: false,
  types: visualizationStore.types,
  width: null,
  height: 150, // FIXME:
  hide() {
    isVisualizationVisible.value = false
  },
  updateType(type) {
    visualizationType.value = type
  },
  isCircularMenuVisible: isCircularMenuVisible.value,
  get nodeSize() {
    return nodeSize.value
  },
})
provideVisualizationConfig(visualizationConfig)

useDocumentEvent('keydown', (event) => {
  if (isInputEvent(event)) {
    return
  }
  if (event.key === ' ') {
    if (event.shiftKey) {
      if (isVisualizationVisible.value) {
        visualizationConfig.value.fullscreen = !visualizationConfig.value.fullscreen
      } else {
        isVisualizationVisible.value = true
        visualizationConfig.value.fullscreen = true
      }
    } else {
      isVisualizationVisible.value = !isVisualizationVisible.value
    }
  }
})

watchEffect(async (onCleanup) => {
  if (isVisualizationVisible.value) {
    let shouldSwitchVisualization = true
    onCleanup(() => {
      shouldSwitchVisualization = false
    })
    const component = await visualizationStore.get(visualizationType.value)
    if (shouldSwitchVisualization) {
      visualization.value = component
      visualizationData.value = queuedVisualizationData.value
    }
  }
})

function onBlur(event: FocusEvent) {
  if (!(event.relatedTarget instanceof Node) || !rootNode.value?.contains(event.relatedTarget)) {
    isCircularMenuVisible.value = false
  }
}

function onExpressionClick(event: Event) {
  if (isInputEvent(event)) {
    return
  }
  rootNode.value?.focus()
  isCircularMenuVisible.value = true
}

watch(
  () => [isAutoEvaluationDisabled.value, isDocsVisible.value, isVisualizationVisible.value],
  () => {
    rootNode.value?.focus()
  },
)

function updatePreprocessor(module: string, method: string, ...args: string[]) {
  console.log(
    `preprocessor changed. node id: ${
      props.node.rootSpan.id
    } module: ${module}, method: ${method}, args: [${args.join(', ')}]`,
  )
}
</script>

<template>
  <div
    ref="rootNode"
    :tabindex="-1"
    class="GraphNode"
    :style="{ transform }"
    :class="{ dragging: dragPointer.dragging }"
    @focus="isCircularMenuVisible = true"
    @blur="onBlur"
  >
    <div class="binding" @pointerdown.stop>
      {{ node.binding }}
    </div>
    <CircularMenu
      v-if="isCircularMenuVisible"
      v-model:is-auto-evaluation-disabled="isAutoEvaluationDisabled"
      v-model:is-docs-visible="isDocsVisible"
      v-model:is-visualization-visible="isVisualizationVisible"
    />
    <component
      :is="visualization"
      v-if="isVisualizationVisible && visualization"
      :data="visualizationData"
      @update:preprocessor="updatePreprocessor"
      @update:type="visualizationType = $event"
    />
    <div class="node" v-on="dragPointer.events" @click.stop="onExpressionClick">
      <SvgIcon class="icon grab-handle" name="number_input" @pointerdown="handleClick"></SvgIcon>
      <div
        ref="editableRootNode"
        class="editable"
        contenteditable
        spellcheck="false"
        @beforeinput="editContent"
        @focus="isCircularMenuVisible = true"
        @blur="onBlur"
        @pointerdown.stop
      >
        <NodeSpan
          :content="node.content"
          :span="node.rootSpan"
          :offset="0"
          @updateExprRect="updateExprRect"
        />
      </div>
    </div>
  </div>
</template>

<style scoped>
.GraphNode {
  color: red;
  position: absolute;
}

.node {
  position: relative;
  top: 0;
  left: 0;

  caret-shape: bar;

  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  background: #222;
  padding: 4px 8px;
  border-radius: var(--radius-full);
}

.binding {
  user-select: none;
  margin-right: 10px;
  color: black;
  position: absolute;
  right: 100%;
  top: 50%;
  transform: translateY(-50%);
}

.editable {
  outline: none;
  height: 24px;
  padding: 1px 0;
}

.container {
  position: relative;
  display: flex;
  gap: 4px;
}

.grab-handle {
  color: white;
  cursor: grab;
  margin-right: 10px;
}

.GraphNode.dragging,
.GraphNode.dragging .icon {
  cursor: grabbing;
}

.visualization {
  position: absolute;
  top: 100%;
  width: 100%;
  margin-top: 4px;
  padding: 4px;
  background: #222;
  border-radius: 16px;
}
</style>
