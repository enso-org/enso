<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectWidgetRegistry } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { AstExtended } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { computed, onUpdated, shallowRef, watch, type ComponentPublicInstance } from 'vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended }>()

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const navigator = injectGraphNavigator()

const rootNode = shallowRef<HTMLElement | ComponentPublicInstance>()
const filteredRootNode = computed(() => {
  const element = rootNode.value
  if (element instanceof HTMLElement) return element
  else if (element?.$el instanceof HTMLElement) return element.$el
  else return null
})
const exprRect = shallowRef<Rect>()
const nodeSize = useResizeObserver(filteredRootNode, false)
function updateRect() {
  let domNode = filteredRootNode.value
  if (domNode == null) return
  const rect = navigator.clientToSceneRect(Rect.FromDomRect(domNode.getBoundingClientRect()))
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
    ref="rootNode"
    :ast="selectedWidget.ast"
    :nodeSpanStart="props.nodeSpanStart"
  />
</template>
