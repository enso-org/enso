<script setup lang="ts">
import { defineWidget, Score, widgetProps } from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { provideTopLevelArgument } from '@/providers/topLevelArgument'
import { ApplicationKind, ArgumentInfoKey } from '@/util/callTree'
import { useTemplateRef } from 'vue'

defineProps(widgetProps(widgetDefinition))

provideTopLevelArgument(useTemplateRef('content'))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  ArgumentInfoKey,
  {
    priority: -1,
    score: (props) =>
      props.nesting < 2 && props.input[ArgumentInfoKey].appKind === ApplicationKind.Prefix ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTopLevelArgument widgetParent widgetExpanded widgetResetPadding">
    <!-- 
      Element used as a reference for `topLevelArgument` context provider, without vertical line.
      (because we don't want it to be considered by subwidgets like WidgetSelection).
    -->
    <div ref="content" class="widgetParent">
      <NodeWidget :input="input" />
    </div>
  </div>
</template>

<style scoped>
.WidgetTopLevelArgument {
  &:before {
    content: '';
    display: block;
    align-self: stretch;
    margin-top: -4px;
    margin-bottom: -4px;
    margin-right: var(--widget-token-pad-unit);
    border-left: 1px solid rgb(0 0 0 / calc(0.12 * var(--size-transition-progress, 1)));
  }
}
</style>
