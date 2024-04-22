<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry';
import { SelectionArrowKey } from './WidgetSelection.vue';

const props = defineProps(widgetProps(widgetDefinition))
const innerInput = { ...props.input }
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 51,
    score: (props) =>
      props.input[SelectionArrowKey] === true ? Score.Perfect : Score.Mismatch
  },
  import.meta.hot,
)
</script>

<template>
  <div class=".WidgetSelectionArrow">
    <NodeWidget :input="innerInput" />
    <SvgIcon v-if="true" name="arrow_right_head_only" class="arrow" />
  </div>
</template>

<style scoped>
.arrow {
  position: absolute;
  pointer-events: none;
  bottom: -7px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
}
</style>
