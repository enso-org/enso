<script setup lang="ts">
import { computed } from 'vue'

const props = defineProps<{
  size?: number
}>()
const dynStyle = computed(() => {
  const size = props.size ?? 30
  return {
    width: `${size}px`,
    height: `${size}px`,
  }
})
</script>

<template>
  <div class="LoadingSpinner" :style="dynStyle"></div>
</template>

<style scoped>
.LoadingSpinner {
  border-radius: 50%;
  border: 4px solid;
  border-color: rgba(0, 0, 0, 30%) #0000;
  animation: s1 0.8s infinite;
}
@keyframes s1 {
  to {
    transform: rotate(0.5turn);
  }
}
</style>
