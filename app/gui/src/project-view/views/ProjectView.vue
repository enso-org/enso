<script setup lang="ts">
import GraphEditor from '@/components/GraphEditor.vue'
import { provideProjectStore } from '@/stores/project'
import { provideSettings } from '@/stores/settings'

const props = defineProps<{ projectId: string; renameProject: (newName: string) => void }>()

provideProjectStore(props)
provideSettings()
</script>

<template>
  <GraphEditor />
</template>

<style scoped></style>
