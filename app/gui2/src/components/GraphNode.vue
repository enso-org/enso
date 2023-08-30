<script setup lang="ts">
import type { ContentRange, ExprId, Node } from '@/stores/graph'
import { Rect } from '@/stores/rect'
import { useResizeObserver } from '@/util/events'
import { computed, onUpdated, reactive, ref, watch, watchEffect } from 'vue'
import NodeSpan from './NodeSpan.vue'

const props = defineProps<{
  node: Node
}>()

const emit = defineEmits<{
  (event: 'updateNodeRect', rect: Rect): void
  (event: 'updateExprRect', id: ExprId, rect: Rect): void
  (event: 'updateNodeContent', range: ContentRange, content: string): void
}>()

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)
const editableRoot = ref<HTMLElement>()

watchEffect(() => {
  const size = nodeSize.value
  if (!size.isZero()) {
    emit('updateNodeRect', new Rect(props.node.position, nodeSize.value))
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
  range.start = updateOffset(range.start, threhsold, adjust)
  range.end = updateOffset(range.end, threhsold, adjust)
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
  const ranges = domRanges.map((r) => {
    return {
      start: getRelatedSpanOffset(r.startContainer, r.startOffset),
      end: getRelatedSpanOffset(r.endContainer, r.endOffset),
    }
  })
  console.log(domRanges, ranges)
  switch (e.inputType) {
    case 'insertText': {
      const content = e.data ?? ''
      for (let range of ranges) {
        if (range.start != range.end) {
          editsToApply.push({ range, content: '' })
        }
        editsToApply.push({ range: { start: range.end, end: range.end }, content })
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
    const adjust = content.length - (range.end - range.start)
    editsToApply.forEach((e) => updateRange(e.range, range.end, adjust))
    if (selectionToRecover) {
      selectionToRecover.ranges.forEach((r) => updateRange(r, range.end, adjust))
      if (selectionToRecover.anchor != null) {
        selectionToRecover.anchor = updateOffset(selectionToRecover.anchor, range.end, adjust)
      }
      if (selectionToRecover.focus != null) {
        selectionToRecover.focus = updateOffset(selectionToRecover.focus, range.end, adjust)
      }
    }
    emit('updateNodeContent', range, content)
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
    .map((r) => {
      const start = getRelatedSpanOffset(r.startContainer, r.startOffset)
      const end = getRelatedSpanOffset(r.endContainer, r.endOffset)
      return { start, end }
    })

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
  if (selectionToRecover != null && editableRoot.value != null) {
    const saved = selectionToRecover
    const root = editableRoot.value
    selectionToRecover = null
    const selection = window.getSelection()
    if (selection == null) return

    function findTextNodeAtOffset(offset: number | null): { node: Text; offset: number } | null {
      if (offset == null) return null
      for (let textSpan of root.querySelectorAll<HTMLSpanElement>('span[data-span-start]')) {
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

    for (let range of saved.ranges) {
      const start = findTextNodeAtOffset(range.start)
      const end = findTextNodeAtOffset(range.end)
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
</script>

<template>
  <div class="Node" ref="rootNode" :style="{ transform }">
    <div class="binding">{{ node.binding }}</div>
    <div
      class="editable"
      contenteditable
      ref="editableRoot"
      @beforeinput="editContent"
      spellcheck="false"
    >
      <NodeSpan :content="node.content" :span="node.rootSpan" @updateExprRect="updateExprRect" />
    </div>
  </div>
</template>

<style scoped>
.Node {
  position: absolute;
  top: 0;
  left: 0;

  caret-shape: bar;

  display: flex;
  flex-direction: row;
  align-items: center;
  white-space: nowrap;
  background: #222;
  padding: 5px 10px;
  border-radius: 20px;
}

.binding {
  color: #ccc;
  margin-right: 10px;
  position: absolute;
  right: 100%;
  top: 50%;
  transform: translateY(-50%);
}

.editable {
  outline: none;
}
</style>
