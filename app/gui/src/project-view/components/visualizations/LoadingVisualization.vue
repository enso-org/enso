<script setup lang="ts">
import '#/tailwind.css'

import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
</script>

<template>
  <div class="LoadingVisualization">
    <LoadingSpinner phase="loading-medium" />
  </div>
</template>

<style scoped>
.LoadingVisualization {
  display: flex;
  height: 100%;
  place-content: center;
  place-items: center;
  overflow: clip;
}
</style>
