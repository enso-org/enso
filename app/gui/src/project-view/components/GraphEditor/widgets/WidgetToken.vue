<script setup lang="ts">
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import { computed } from 'vue'
import { Token as RawToken } from 'ydoc-shared/ast/generated/ast'

const { input } = defineProps(widgetProps(widgetDefinition))

const substitutes: Partial<
  Record<(typeof RawToken.typeNames)[number] | 'Raw', Map<string, string>>
> = {
  TextStart: new Map([
    ['"', '\u201C'],
    ['"""', '\u201C\u201C\u201C'],
    ["'", '\u2018'],
    ["'''", '\u2018\u2018\u2018'],
  ]),
  TextEnd: new Map([
    ['"', '\u201D'],
    ["'", '\u2019'],
  ]),
}

const spanClass = computed(() => input.value.typeName)
const rawContent = computed(() => input.value.code())
const displayContent = computed(
  () => substitutes[input.value.typeName]?.get(rawContent.value) ?? rawContent.value,
)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isToken,
  {
    priority: 0,
    score: Score.Good,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetToken widgetSingleLine widgetApplyPadding" :class="spanClass">
    {{ displayContent }}
  </div>
</template>

<style scoped>
.WidgetToken {
  white-space: pre;
  opacity: 0.33;

  &.Ident,
  &.TextSection,
  &.Digits {
    opacity: 1;
  }

  &.TextSection,
  &.Digits {
    font-weight: bold;
  }
}
</style>
