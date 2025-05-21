<script setup lang="ts">
import { DocumentationMetadata } from '@/components/DocumentationPanel/metadata'

const props = defineProps<{
  metadata: DocumentationMetadata
}>()
</script>

<template>
  <div class="FrontMatter">
    {{ 'Start' }}
    {{ JSON.stringify(metadata) }}
    {{ 'End' }}
  </div>
</template>

<style scoped>
.FrontMatter {
}
</style>
