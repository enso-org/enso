<script setup lang="ts">
import { Score, defineWidget, type WidgetProps } from '@/providers/widgetRegistry'
import { Placeholder } from '@/util/ast/abstract.ts'
const _props = defineProps<WidgetProps>()
</script>

<script lang="ts">
export const widgetDefinition = defineWidget({
  priority: 10,
  match: (props) => (props.input instanceof Placeholder ? Score.Good : Score.Mismatch),
})
</script>

<template>
  <span ref="rootNode" class="WidgetBlank">_</span>
</template>

<style scoped>
.WidgetBlank {
  color: transparent;
  display: inline-block;
  position: relative;
  width: 20px;
  &::before {
    content: '';
    display: block;
    position: absolute;
    width: 20px;
    height: 4px;
    border-radius: 2px;
    bottom: 0;
    background-color: var(--node-color-port);
    transition: background-color 0.2s ease;
  }
}
</style>
