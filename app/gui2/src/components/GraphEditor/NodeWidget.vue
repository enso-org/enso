<script setup lang="ts">
import { injectWidgetRegistry } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { AstExtended } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { computed, onUpdated, shallowRef, watch } from 'vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended }>()

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()

const rootNode = shallowRef<HTMLElement>()
const filteredRootNode = computed(() => {
  const element = rootNode.value
  if (element instanceof HTMLElement) return element
  else return null
})
const exprRect = shallowRef<Rect>()
const nodeSize = useResizeObserver(filteredRootNode, false)
function updateRect() {
  let domNode = rootNode.value
  if (domNode == null) return
  const rect = Rect.FromDomRect(domNode.getBoundingClientRect())
  console.log('rect', rect)
  if (exprRect.value != null && rect.equals(exprRect.value)) return
  exprRect.value = rect
}
watch(nodeSize, updateRect)
onUpdated(updateRect)
watch(exprRect, (rect) => rect && tree.updateRect(props.ast.astId, rect))

const selectedWidget = computed(() => registry.select(props.ast))
</script>

<template>
  <component
    :is="selectedWidget.component"
    :ast="selectedWidget.ast"
    :nodeSpanStart="props.nodeSpanStart"
  />
</template>
