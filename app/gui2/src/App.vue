<script setup lang="ts">
import { onMounted } from 'vue'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import ProjectView from '@/views/ProjectView.vue'

onMounted(() => useSuggestionDbStore().initializeDb())
</script>

<template>
  <ProjectView class="flex" />
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
