<script setup lang="ts">
import { provideWidgetTree } from '@/providers/widgetTree'
import type { AstExtended } from '@/util/ast'
import { computed } from 'vue'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{
  ast: AstExtended
}>()

const spanStart = computed(() => props.ast.span()[0])
provideWidgetTree(spanStart)
</script>

<template>
  <NodeWidget :ast="ast" />
</template>
