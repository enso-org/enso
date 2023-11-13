<script setup lang="ts">
import { Score, defineWidget, widgetAst, type WidgetProps } from '@/providers/widgetRegistry'
import { computed, ref } from 'vue'

const props = defineProps<WidgetProps>()

const rootNode = ref<HTMLElement>()

const spanClass = computed(() => widgetAst(props.input)?.tokenTypeName())
const repr = computed(() => widgetAst(props.input)?.repr())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 1000,
  match: (info) => (widgetAst(info.input)?.isToken() ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span ref="rootNode" :class="['Token', spanClass]">{{ repr }}</span>
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
