<script setup lang="ts">
import type { AstExtended } from '@/util/ast'
import NodeWidget from './NodeWidget.vue'

const props = defineProps<{ ast: AstExtended }>()
</script>

<template>
  <NodeWidget :ast="props.ast" />
</template>
