<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { ArgumentAst, ArgumentPlaceholder } from '@/util/callTree'
import { isInstance } from '@/util/predicates'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [isInstance(ArgumentAst), isInstance(ArgumentPlaceholder)],
  {
    priority: -1,
    score: (props) => (props.nesting < 2 ? Score.Perfect : Score.Mismatch),
  },
)
</script>

<template>
  <span class="WidgetTopLevelArgument">
    <NodeWidget :input="props.input" nest />
  </span>
</template>

<style scoped>
.WidgetTopLevelArgument {
  display: inline-flex;
  flex-direction: row;
  place-items: center;
  position: relative;
  height: var(--node-height);

  &:before {
    content: '';
    display: block;
    margin: 0 8px 0 4px;
    height: var(--node-height);
    border-left: 1px solid rgb(0 0 0 / 0.12);
  }
}
.name {
  color: rgb(255 255 255 / 0.5);
  margin-right: 4px;
}
</style>
