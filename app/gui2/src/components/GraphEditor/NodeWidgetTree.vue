<script setup lang="ts">
import { nodeEditBindings } from '@/bindings'
import { provideWidgetTree } from '@/providers/widgetTree'
import { useTransitioning } from '@/util/animation'
import type { AstExtended } from '@/util/ast'
import { Vec2 } from '@/util/vec2'
import { ref, toRef } from 'vue'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{ ast: AstExtended }>()
const emit = defineEmits<{
  'update:edited': [cursorPosition: number]
}>()

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
        'Neither caretPositionFromPoint nor caretRangeFromPoint are supported by this browser',
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
  return 0
}

const observedLayoutTransitions = new Set([
  'margin-left',
  'margin-right',
  'margin-top',
  'margin-bottom',
  'padding-left',
  'padding-right',
  'padding-top',
  'padding-bottom',
  'width',
  'height',
])

const layoutTransitions = useTransitioning(observedLayoutTransitions)
provideWidgetTree(toRef(props, 'ast'), layoutTransitions.active)
</script>

<template>
  <span class="NodeWidgetTree" spellcheck="false" v-on="layoutTransitions.events">
    <NodeWidget :input="ast" />
  </span>
</template>

<style scoped>
.NodeWidgetTree {
  color: white;
  margin-left: 4px;
}
</style>
