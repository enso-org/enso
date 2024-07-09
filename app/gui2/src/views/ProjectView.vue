<script setup lang="ts">
import GraphEditor from '@/components/GraphEditor.vue'
import { provideProjectStore } from '@/stores/project'

const props = defineProps<{ projectId: string; renameProject: (newName: string) => void }>()

provideProjectStore(props)
</script>

<template>
  <GraphEditor />
</template>

<style scoped></style>
