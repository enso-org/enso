<script setup lang="ts">
import { provideGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import ProjectView from '@/views/ProjectView.vue'
import { toRef } from 'vue'

const props = defineProps<{
  config: GuiConfig
}>()

provideGuiConfig(toRef(props, 'config'))
</script>

<template>
  <ProjectView class="flex" />
</template>

<style scoped>
.flex {
  flex: 1;
}
</style>
@/providers/guiConfig
