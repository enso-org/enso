<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry';
import { injectSelectionArrow } from '@/providers/selectionArrow';
import { Ast } from '@/util/ast';

const props = defineProps(widgetProps(widgetDefinition))
const innerInput = { ...props.input }
const info = injectSelectionArrow(true)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 51,
    score: (props) => {
      const info = injectSelectionArrow(true)
      if (props.input.value instanceof Ast.Ident && props.input.value.token.id === info?.id) return Score.Perfect
      if (props.input.value instanceof Ast.Ast && props.input.value.id === info?.id) return Score.Perfect
      return info?.id === props.input.portId ? Score.Perfect : Score.Mismatch
    }
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelectionArrow">
    <NodeWidget :input="innerInput" allowEmpty />
    <SvgIcon v-if="info?.hovered" name="arrow_right_head_only" class="arrow" />
  </div>
</template>

<style scoped>
.WidgetSelectionArrow {
  position: relative;
}

.arrow {
  position: absolute;
  pointer-events: none;
  bottom: -11px;
  left: 50%;
  transform: translateX(50%) rotate(90deg) scale(0.7);
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
}
</style>
