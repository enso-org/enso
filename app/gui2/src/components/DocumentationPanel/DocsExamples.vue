<script setup lang="ts">
import type { Example } from '@/components/DocumentationPanel/ir'

const props = defineProps<{ examples: Example[] }>()
</script>

<template>
  <div v-if="props.examples.length > 0">
    <div v-for="(example, index) in props.examples" :key="index" class="exampleContainer">
      <!-- eslint-disable-next-line vue/no-v-html -->
      <div v-html="example.body"></div>
    </div>
  </div>
</template>

<style scoped>
.exampleContainer {
  background-color: var(--enso-docs-example-background-color);
  border-radius: 0.25rem;
  padding: 0.5rem;
  margin-bottom: 0.5rem;
}

:deep(.example) {
  white-space: pre;
  overflow-x: auto;
  margin: 0.05rem 0.1rem;
  box-shadow: inset 0 0 1px 0.5px rgba(0, 0, 0, 0.2);
}
</style>
