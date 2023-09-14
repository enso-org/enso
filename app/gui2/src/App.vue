<script setup lang="ts">
import { onMounted } from 'vue'
import { initializeMockDb } from '@/stores/suggestionDatabase'
import ProjectView from '@/views/ProjectView.vue'

onMounted(initializeMockDb)
</script>

<template>
  <ProjectView class="flex" />
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
