<script setup lang="ts">
import GraphEditor from '@/components/GraphEditor.vue'
import { provideProjectStore } from '@/stores/project'

const props = defineProps<{ renameProject: (newName: string) => void }>()

provideProjectStore(props.renameProject)
</script>

<template>
  <GraphEditor />
</template>

<style scoped></style>
