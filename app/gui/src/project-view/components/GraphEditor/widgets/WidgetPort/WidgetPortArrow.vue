<script setup lang="ts">
import { EDGE_ARROW_PATH } from '@/components/GraphEditor/GraphEdges.vue'
const _props = defineProps<{
  /** Whether the arrow is enabled and should be displayed. */
  enabled: boolean
  /** Whether the arrow should be temporarily hidden (visually). */
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
    <svg v-if="enabled" class="clickable" viewBox="0 0 12 9">
      <path :d="EDGE_ARROW_PATH" />
    </svg>
  </div>
</template>

<style scoped>
.WidgetPortArrow {
  position: absolute;
  color: var(--color-missing-value);
  top: -16px;
  left: 50%;
  transform: translateX(-50%) scale(0.5);
  transition: transform 0.2s ease;
  fill: currentColor;
  &:hover {
    transform: translateX(-50%) translateY(-2px) scale(0.6);
  }

  &.hide {
    color: rgba(0, 0, 0, 0);
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
