<script setup lang="ts">
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { AstExtended } from '@/util/ast'
import { computed, ref } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const rootNode = ref<HTMLElement>()

const spanClass = computed(() => props.input.tokenTypeName())
const repr = computed(() => props.input.repr())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AstExtended.isToken(), {
  priority: 1000,
  score: Score.Good,
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
