<script setup lang="ts">
import type { Vec2 } from '@/util/vec2'

const props = defineProps<{
  start: Vec2
  size: Vec2
}>()
</script>

<template>
  <div
    class="SelectionBrush"
    :style="{
      left: `${start.x + Math.min(size.x, 0)}px`,
      top: `${start.y + Math.min(size.y, 0)}px`,
      width: `${Math.abs(size.x)}px`,
      height: `${Math.abs(size.y)}px`,
    }"
  ></div>
</template>

<style scoped>
.SelectionBrush {
  box-sizing: content-box;
  position: absolute;
  pointer-events: none;
  background: lch(70% 0 0 / 0.5);
  border-radius: var(--radius-cursor);
  border: var(--radius-cursor) solid #0000;
  margin: calc(0px - var(--radius-cursor));
}
</style>
