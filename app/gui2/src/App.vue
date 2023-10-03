<script setup lang="ts">
import { provideGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import ProjectView from '@/views/ProjectView.vue'
import { onMounted, toRef } from 'vue'

const props = defineProps<{
  config: GuiConfig
}>()

provideGuiConfig(toRef(props, 'config'))

// Initialize suggestion db immediately, so it will be ready when user needs it.
onMounted(() => useSuggestionDbStore())
</script>

<template>
  <ProjectView class="flex" />
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
