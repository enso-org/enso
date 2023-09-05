<script setup lang="ts">
import { Suspense } from 'vue'
import ProjectView from './views/ProjectView.vue'
</script>

<template>
  <Suspense>
    <ProjectView class="flex" />
  </Suspense>
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
