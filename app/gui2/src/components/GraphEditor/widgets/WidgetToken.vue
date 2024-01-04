<script setup lang="ts">
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const spanClass = computed(() => props.input.ast.typeName())
const repr = computed(() => props.input.ast.code())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isToken, {
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
