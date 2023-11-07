<script setup lang="ts">
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { injectGraphSelection } from '@/providers/graphSelection'
import { injectWidgetRegistry } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { injectWidgetUsageInfo, provideWidgetUsageInfo } from '@/providers/widgetUsageInfo'
import { useGraphStore } from '@/stores/graph'
import { Ast, type AstExtended } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import {
  computed,
  onUpdated,
  ref,
  shallowRef,
  toRef,
  watch,
  watchEffect,
  type ComponentPublicInstance,
} from 'vue'

const props = defineProps<{ ast: AstExtended }>()

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const navigator = injectGraphNavigator()
const nodeSelection = injectGraphSelection(true)
const graph = useGraphStore()

const isHovered = ref(false)
const reactToHover = computed(() => isHoverable() && isHovered.value)

watchEffect((onCleanup) => {
  if (nodeSelection != null && reactToHover.value === true) {
    nodeSelection.hoveredExpr = props.ast.astId
    onCleanup(() => {
      nodeSelection.hoveredExpr = undefined
    })
  }
})

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
  if (!isHoverable()) return
  let exprDomNode = filteredRootNode.value
  const rootDomNode = exprDomNode?.closest('.node')
  if (exprDomNode == null || rootDomNode == null) return

  const exprClientRect = Rect.FromDomRect(exprDomNode.getBoundingClientRect())
  const nodeClientRect = Rect.FromDomRect(rootDomNode.getBoundingClientRect())
  const exprSceneRect = navigator.clientToSceneRect(exprClientRect)
  const exprNodeRect = navigator.clientToSceneRect(nodeClientRect)
  const localRect = exprSceneRect.offsetBy(exprNodeRect.pos.inverse())
  if (exprRect.value != null && localRect.equals(exprRect.value)) return
  exprRect.value = localRect
}

watch(nodeSize, updateRect)
onUpdated(updateRect)

const rectUpdateIsUseful = computed(() => {
  const id = props.ast.astId
  return isHoverable() && (isHovered.value || graph.db.connections.reverseLookup(id).size > 0)
})

watch(
  () => [props.ast.astId, exprRect.value, rectUpdateIsUseful.value] as const,
  ([id, rect, updateUseful], _, onCleanup) => {
    if (rect != null && updateUseful) {
      graph.updateExprRect(id, rect)
      onCleanup(() => {
        if (props.ast.astId === id && rect === exprRect.value) graph.updateExprRect(id, undefined)
      })
    }
  },
)

// Return whether this node should interact with the mouse, e.g. when seeking an edge target.
function isHoverable(): boolean {
  if (props.ast.isToken()) return false
  switch (props.ast.inner.type) {
    case Ast.Tree.Type.Invalid:
    case Ast.Tree.Type.BodyBlock:
    case Ast.Tree.Type.Ident:
    case Ast.Tree.Type.Number:
    case Ast.Tree.Type.UnaryOprApp:
    case Ast.Tree.Type.Wildcard:
    case Ast.Tree.Type.TextLiteral:
      return true
    default:
      return false
  }
}

const parentUsageInfo = injectWidgetUsageInfo(true)
const whitespace = computed(() =>
  parentUsageInfo?.ast !== props.ast ? ' '.repeat(props.ast.whitespaceLength()) : '',
)
const selectedWidget = computed(() => {
  const alreadyUsed = parentUsageInfo?.ast === props.ast ? parentUsageInfo.set : undefined
  return registry.select(props.ast, alreadyUsed)
})
provideWidgetUsageInfo(parentUsageInfo, toRef(props, 'ast'), selectedWidget)
const spanStart = computed(
  () => props.ast.span()[0] - tree.nodeSpanStart.value - whitespace.value.length,
)
</script>

<template>
  <span v-if="whitespace.length > 0">{{ whitespace }}</span
  ><component
    :is="selectedWidget"
    ref="rootNode"
    :ast="props.ast"
    :data-span-start="spanStart"
    @pointerenter="isHovered = true"
    @pointerleave="isHovered = false"
  />
</template>
