<script setup lang="ts">
import { Ast, type AstExtended } from '@/util/ast'
import { useResizeObserver } from '@/util/events'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import type { ExprId } from 'shared/yjsModel'
import { computed, onUpdated, ref, shallowRef, watch } from 'vue'

const props = defineProps<{ nodeSpanStart: number; ast: AstExtended<Ast.Token> }>()

const emit = defineEmits<{
  updateExprRect: [expr: ExprId, rect: Rect]
}>()

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode, false)
const exprRect = shallowRef<Rect>()

const spanClass = computed(() => Ast.Token.typeNames[props.ast.inner.type])
const whitespace = computed(() => ' '.repeat(props.ast.inner.whitespaceLengthInCodeBuffer))

function updateRect() {
  let domNode = rootNode.value
  if (domNode == null) return
  const pos = new Vec2(domNode.offsetLeft, domNode.offsetTop)
  const size = nodeSize.value
  const rect = new Rect(pos, size)
  if (exprRect.value != null && rect.equals(exprRect.value)) return
  exprRect.value = rect
}

watch(nodeSize, updateRect)
onUpdated(updateRect)
watch(exprRect, (rect) => rect && emit('updateExprRect', props.ast.astId, rect))
</script>

<template>
  <span
    ref="rootNode"
    :class="['Token', spanClass]"
    :data-span-start="props.ast.span()[0] - nodeSpanStart - whitespace.length"
    >{{ whitespace }}{{ props.ast.repr() }}</span
  >
</template>

<style scoped>
.Token {
  color: white;
  white-space: pre;
  align-items: center;
  color: rgb(255 255 255 / 0.33);

  &.Ident,
  &.TextSection,
  &.Digits {
    color: white;
  }

  &.TextSection,
  &.Digits {
    font-weight: bold;
  }
}
</style>
