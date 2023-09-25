<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

const props = defineProps<{
  x: number
  y: number
}>()
</script>

<template>
  <SvgIcon class="CustomCursor" name="cursor" :style="{ left: x, top: y }" />
</template>

<style scoped>
.CustomCursor {
  pointer-events: none;
  margin: calc(0px - var(--radius-cursor));
}
</style>
