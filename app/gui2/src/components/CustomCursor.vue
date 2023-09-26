<script setup lang="ts">
const props = defineProps<{
  x: number
  y: number
}>()
</script>

<template>
  <div class="CustomCursor" :style="{ left: `${x}px`, top: `${y}px` }" />
</template>

<style scoped>
.CustomCursor {
  pointer-events: none;
  position: absolute;
  height: calc(var(--radius-cursor) * 2);
  width: calc(var(--radius-cursor) * 2);
  margin: calc(0px - var(--radius-cursor));
  border-radius: var(--radius-full);
  background: lch(70% 0 0 / 0.5);
}
</style>
