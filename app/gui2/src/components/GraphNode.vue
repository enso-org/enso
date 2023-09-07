<script setup lang="ts">
import CircularMenu from '@/components/CircularMenu.vue'
import NodeSpan from '@/components/NodeSpan.vue'

import type { Node } from '@/stores/graph'
import { Rect } from '@/stores/rect'
import { usePointer, useResizeObserver } from '@/util/events'
import type { ContentRange, ExprId } from 'shared/yjs-model'
import type { Vec2 } from '@/util/vec2'

import { computed, onUpdated, reactive, ref, shallowRef, watch, watchEffect } from 'vue'

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

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)
const editableRoot = ref<HTMLElement>()

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
  const root = editableRoot.value
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
  const root = editableRoot.value

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

  if (selectionToRecover != null && editableRoot.value != null) {
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

// FIXME: this should default to `false`
const isCircularMenuVisible = ref(true)

const isAutoEvaluationDisabled = ref(false)
const isDocsVisible = ref(false)
const isVisualizationVisible = ref(false)

type VisualizationModule = typeof import('visualizations/VisualizationContainer.vue') & {
  name?: string
  inputType?: string
}

// FIXME: statically resolved imports will not work for user-defined components
const VISUALIZATION_GETTERS: Record<string, () => Promise<VisualizationModule>> = {
  Warnings: () => import('visualizations/WarningsVisualization.vue'),
  Bubble: () => import('visualizations/BubbleVisualization.vue'),
  'Image (Base64)': () => import('visualizations/ImageBase64Visualization.vue'),
  GeoMap: () => import('visualizations/GeoMapVisualization.vue'),
  Scatterplot: () => import('visualizations/ScatterplotVisualization.vue'),
} as any

const visualizationType = ref('Warnings')
const visualization =
  shallowRef<typeof import('visualizations/VisualizationContainer.vue').default>()
const visualizationTypes = computed(() =>
  Object.keys(VISUALIZATION_GETTERS).filter((type) => type !== visualizationType.value),
)

const queuedVisualizationData = computed<{}>(() => {
  switch (visualizationType.value) {
    case 'Warnings': {
      return ['warning 1', "warning 2!!&<>;'\x22"]
    }
    case 'Bubble': {
      return [
        [1, 1, 1],
        [2, 2, 2],
        [3, 3, 3],
        [5, 5, 5],
        [10, 10, 10],
        [10, 100, 10],
        [100, 10, 10],
        [100, 100, 10],
      ]
    }
    default: {
      return {}
    }
  }
})
const visualizationData = ref<{}>({})

function registerVisualization(name: string, inputType: string) {
  console.log(`registering visualization: name=${name}, inputType=${inputType}`)
}

// NOTE: Because visualization scripts are cached, they are not guaranteed to be up to date.
const VISUALIZATION_TYPES: Record<string, any> = {}
watchEffect(async (onCleanup) => {
  let shouldSwitchVisualization = true
  onCleanup(() => {
    shouldSwitchVisualization = false
  })
  const currentVisualizationType = visualizationType.value
  let component: VisualizationModule['default'] = VISUALIZATION_TYPES[currentVisualizationType]
  if (component == null) {
    const module = await VISUALIZATION_GETTERS[currentVisualizationType]()
    // TODO: fallback to name based on path to visualization.
    registerVisualization(module.name ?? currentVisualizationType, module.inputType ?? 'Any')
    component = module.default
    VISUALIZATION_TYPES[currentVisualizationType] = component
  }
  if (shouldSwitchVisualization) {
    visualization.value = component
    visualizationData.value = queuedVisualizationData.value
  }
})

const visualizationWidth = ref<number>()
const visualizationHeight = ref<number>()
const isVisualizationFullscreen = ref(false)
</script>

<template>
  <div class="GraphNode" :style="{ transform }">
    <div
      class="binding"
      @click="isVisualizationVisible = !isVisualizationVisible"
      @pointerdown.stop
    >
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
      v-model:width="visualizationWidth"
      v-model:height="visualizationHeight"
      v-model:fullscreen="isVisualizationFullscreen"
      :types="visualizationTypes"
      :data="visualizationData"
      :is-circular-menu-visible="isCircularMenuVisible"
      @update:preprocessor="
        (module, method, ...args) =>
          console.log(
            `preprocessor changed. module: ${module}, method: ${method}, args: [${args.join(
              ', ',
            )}]`,
          )
      "
      @update:type="visualizationType = $event"
    />
    <div
      ref="rootNode"
      class="node"
      :class="{ dragging: dragPointer.dragging }"
      v-on="dragPointer.events"
    >
      <div class="icon" @pointerdown="handleClick">@ &nbsp;</div>
      <div
        ref="editableRoot"
        class="editable"
        contenteditable
        spellcheck="false"
        @beforeinput="editContent"
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
  padding: 5px 10px;
  border-radius: var(--radius-full);
}

.binding {
  user-select: none;
  margin-right: 10px;
  position: absolute;
  right: 100%;
  top: 50%;
  transform: translateY(-50%);
}

.editable {
  outline: none;
  margin: 4px 8px;
}

.container {
  position: relative;
}

.icon {
  color: white;
  cursor: grab;
}

.Node.dragging,
.Node.dragging .icon {
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
