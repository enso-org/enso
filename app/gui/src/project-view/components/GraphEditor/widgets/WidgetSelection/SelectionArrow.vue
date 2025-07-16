<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

defineProps<{
  location: HTMLElement | null
  isHovered: boolean
}>()
</script>

<template>
  <Teleport :disabled="!location" :to="location">
    <SvgIcon
      name="arrow_right_head_only"
      class="arrow widgetOutOfLayout"
      :class="{ hovered: isHovered }"
    />
  </Teleport>
</template>

<style scoped>
svg.arrow {
  position: absolute;
  bottom: -8px;
  left: 50%;
  transform: translateX(-50%) rotate(90deg) scale(0.7);
  transform-origin: center;
  opacity: 0.5;
  /* Prevent the parent from receiving a pointerout event if the mouse is over the arrow, which causes flickering. */
  pointer-events: none;
  transition: opacity 0.2s ease-in-out;
  &.hovered {
    opacity: 0.9;
  }
}
</style>
