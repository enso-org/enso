<script setup lang="ts">
import { spanKindName, type Span, type ExprId } from '@/stores/graph'
import { Rect } from '@/stores/rect'
import { useResizeObserver } from '@/util/events'
import { Vec2 } from '@/util/vec2'
import { computed, onUpdated, ref, shallowRef, watch } from 'vue'

const props = defineProps<{
  content: string
  span: Span
}>()

const emit = defineEmits<{
  (event: 'updateExprRect', expr: ExprId, rect: Rect): void
}>()

const spanClass = computed(() => spanKindName(props.span.kind))

const exprPart = computed(() => {
  const range = props.span.range
  return props.content.substring(range.start, range.end)
})

const rootNode = ref<HTMLElement>()
const nodeSize = useResizeObserver(rootNode)

const exprRect = shallowRef<Rect>()

function updateRect() {
  let node = rootNode.value
  if (node == null) return
  const pos = new Vec2(node.offsetLeft, node.offsetTop)
  const size = nodeSize.value
  const rect = new Rect(pos, size)
  if (exprRect.value != null && rect.equals(exprRect.value)) return
  exprRect.value = rect
}

watch(nodeSize, updateRect)

onUpdated(() => {
  updateRect()
})

watch(exprRect, (rect) => {
  if (rect == null) return
  emit('updateExprRect', props.span.id, rect)
})
</script>

<template>
  <span
    :class="['Span', spanClass]"
    ref="rootNode"
    style="{ transform }"
    :data-span-id="props.span.id"
    :data-span-start="props.span.range.start"
    ><template v-if="props.span.children.length > 0"
      ><NodeSpan
        v-for="child in props.span.children"
        :key="<any>child.id"
        :content="props.content"
        :span="child"
        @updateExprRect="(id, rect) => emit('updateExprRect', id, rect)"
      /> </template
    ><template v-else>{{ exprPart }}</template></span
  >
</template>

<style scoped>
.Span {
  display: inline-block;
  white-space: pre;

  &.Root {
    color: white;
  }

  &.Ident {
    color: #f97;
  }

  &.Token {
    color: #7f7;
  }

  &.Literal {
    color: #77f;
  }

  &.Group {
    color: #ccc;
  }
}
</style>
