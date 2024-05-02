<script setup lang="ts">
import { MilkdownProvider } from '@milkdown/vue'
import { defineAsyncComponent } from 'vue'

const documentation = defineModel<string>({ required: true })

const LazyMilkdownEditor = defineAsyncComponent(
  () => import('@/components/DocumentationEditor/MilkdownEditor.vue'),
)
</script>

<template>
  <Suspense>
    <MilkdownProvider>
      <LazyMilkdownEditor v-model="documentation" />
    </MilkdownProvider>
  </Suspense>
</template>
