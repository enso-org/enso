<script setup lang="ts">
import { injectWidgetTree } from '@/providers/widgetTree'
import type { AstExtended } from '@/util/ast'
import { colorFromString } from '@/util/colors'
import { uuidv4 } from 'lib0/random'
import { computed } from 'vue'
import NodeWidget from './NodeWidget.vue'

const tree = injectWidgetTree()

const props = defineProps<{
  ast: AstExtended
}>()

const whitespace = computed(() => ' '.repeat(props.ast.whitespaceLength()))

const style = {
  backgroundColor: colorFromString(uuidv4()),
}
</script>

<template>
  <span v-if="whitespace.length > 0" class="whitespace">{{ whitespace }}</span>
  <NodeWidget :ast="props.ast" :nodeSpanStart="tree.nodeSpanStart.value" :style="style" />
</template>
