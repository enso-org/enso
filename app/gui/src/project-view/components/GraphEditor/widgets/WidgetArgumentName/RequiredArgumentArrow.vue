<script setup lang="ts">
import { EDGE_ARROW_PATH } from '@/components/GraphEditor/GraphEdges.vue'
const _props = defineProps<{
  /**
   * Whether the arrow should be temporarily hidden (visually).
   * It is used to hide the arrow when the port is connected, but avoid losing pointer events.
   */
  hide: boolean
}>()

const emit = defineEmits<{
  arrowClick: [e: PointerEvent]
}>()
</script>

<template>
  <div
    :class="['WidgetPortArrow', { hide }, 'widgetOutOfLayout']"
    @pointerdown.stop="emit('arrowClick', $event)"
  >
    <svg class="clickable" viewBox="0 0 12 9">
      <path :d="EDGE_ARROW_PATH" />
    </svg>
  </div>
</template>

<style scoped>
.WidgetPortArrow {
  position: absolute;
  color: var(--color-missing-value);
  top: -14px;
  left: 50%;
  width: 12px;
  height: 9px;
  transform: translateX(-50%);
  transition: transform 0.2s ease;
  fill: currentColor;
  &:hover {
    transform: translateX(-50%) scale(1.1);
  }

  &.hide {
    opacity: 0;
  }

  svg {
    fill: currentColor;
    display: block;
    width: 100%;
    height: 100%;
  }
}

.GraphEditor.draggingEdge .WidgetPort .WidgetPortArrow {
  pointer-events: all;
}
</style>
