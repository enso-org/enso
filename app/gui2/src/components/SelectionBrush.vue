<script setup lang="ts">
import { ref, watchEffect } from 'vue'

import type { Vec2 } from '@/util/vec2'

const props = defineProps<{
  position: Vec2
  size: Vec2 | undefined
}>()

const transition = ref(false)

watchEffect(() => {
  if (props.size == null) {
    transition.value = true
    setTimeout(() => {
      transition.value = false
    }, 150)
  }
})
</script>

<template>
  <div
    class="SelectionBrush"
    :class="{ cursor: size == null, transition }"
    :style="{
      left: `${position.x - Math.max(size?.x ?? 0, 0)}px`,
      top: `${position.y - Math.max(size?.y ?? 0, 0)}px`,
      width: `${Math.abs(size?.x ?? 0)}px`,
      height: `${Math.abs(size?.y ?? 0)}px`,
    }"
  ></div>
</template>

<style scoped>
.SelectionBrush {
  --radius-cursor: 8px;
  --margin-brush: 6px;
  transition-property: border, margin;
  transition-duration: 100ms;
  box-sizing: content-box;
  position: absolute;
  pointer-events: none;
  background: lch(70% 0 0 / 0.5);
  border-radius: var(--radius-cursor);
  border: var(--margin-brush) solid #0000;
  margin: calc(0px - var(--margin-brush));

  &.cursor {
    border: var(--radius-cursor) solid #0000;
    margin: calc(0px - var(--radius-cursor));

    &.transition {
      transition-property: left, top, width, height;
      transition-duration: 150ms;
    }
  }
}
</style>
