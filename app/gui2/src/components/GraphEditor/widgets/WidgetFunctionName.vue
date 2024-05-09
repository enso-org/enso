<script setup lang="ts">
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const repr = computed(() => props.input.value.code())
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.identifier,
  {
    priority: 0,
    score: (props) => {},
  },
  import.meta.hot,
)
</script>

<template>
  <AutoSizedInput class="FunctionName" />
</template>

<style scoped>
.FunctionName {
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
