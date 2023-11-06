<script setup lang="ts">
import { provideWidgetTree } from '@/providers/widgetTree'
import type { AstExtended } from '@/util/ast'
import type { Rect } from '@/util/rect'
import type { ExprId } from 'shared/yjsModel'
import { computed } from 'vue'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{
  ast: AstExtended
}>()

const emit = defineEmits<{
  updateExprRect: [id: ExprId, rect: Rect]
}>()

const spanStart = computed(() => props.ast.span()[0])
const tree = provideWidgetTree(spanStart)
tree.on('rect', (id, rect) => emit('updateExprRect', id, rect))
</script>

<template>
  <NodeWidget :ast="ast" />
</template>
