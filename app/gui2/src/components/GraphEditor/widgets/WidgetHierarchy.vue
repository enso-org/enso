<script setup lang="ts">
import { injectGraphSelection } from '@/providers/graphSelection'
import { Ast, type AstExtended } from '@/util/ast'
import { computed, ref, watchEffect } from 'vue'
import ChildWidget from '../ChildWidget.vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended<Ast.Tree> }>()

const nodeSelection = injectGraphSelection(true)

const spanClass = computed(() => Ast.Tree.typeNames[props.ast.inner.type])
const children = computed(() => [...props.ast.children()])

// Return whether this node should interact with the mouse, e.g. when seeking an edge target.
function isHoverable(): boolean {
  switch (props.ast.inner.type) {
    case Ast.Tree.Type.Invalid:
    case Ast.Tree.Type.BodyBlock:
    case Ast.Tree.Type.Ident:
    case Ast.Tree.Type.Number:
    case Ast.Tree.Type.Wildcard:
    case Ast.Tree.Type.TextLiteral:
      return true
    default:
      return false
  }
}

const isHovered = ref(false)
const reactToHover = computed(() => isHovered.value && isHoverable())

watchEffect((onCleanup) => {
  if (nodeSelection != null && reactToHover.value === true) {
    nodeSelection.hoveredExpr = props.ast.astId
    onCleanup(() => {
      nodeSelection.hoveredExpr = undefined
    })
  }
})
</script>

<template>
  <span
    :class="['Tree', spanClass]"
    :data-span-start="props.ast.span()[0] - nodeSpanStart"
    @pointerenter="isHovered = true"
    @pointerleave="isHovered = false"
    ><ChildWidget v-for="child in children" :key="child.astId" :ast="child" />
  </span>
</template>

<style scoped>
.Tree {
  color: white;
  white-space: pre;
  align-items: center;
  transition: background 0.2s ease;

  &.Root {
    color: rgb(255 255 255 / 0.33);
  }

  &.Ident,
  &.Literal {
    color: white;
  }

  &.Literal {
    font-weight: bold;
  }

  &.port {
    background-color: var(--node-color-port);
    border-radius: var(--node-border-radius);
    margin: -2px -4px;
    padding: 2px 4px;
  }
}
</style>
