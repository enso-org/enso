<script setup lang="ts">
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
</script>

<template>
  <div class="LoadingVisualization">
    <LoadingSpinner />
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
