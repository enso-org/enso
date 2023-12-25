<script setup lang="ts">
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed, ref } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.typeName())
const repr = computed(() => props.input.code())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget((expression) => expression instanceof Ast.Token, {
  priority: 1000,
  score: Score.Good,
})
</script>

<template>
  <span class="WidgetToken" :class="spanClass">{{ repr }}</span>
</template>

<style scoped>
.WidgetToken {
  display: inline-block;
  vertical-align: middle;
  white-space: pre;
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
