<script setup lang="ts">
import { MilkdownProvider } from '@milkdown/vue'
import { defineAsyncComponent } from 'vue'

const documentation = defineModel<string>({ required: true })
const editing = defineModel<boolean>('editing', { default: false })
const props = defineProps<{
  preferSingleLine?: boolean | undefined
}>()

const LazyMilkdownEditor = defineAsyncComponent(
  () => import('@/components/DocumentationEditor/MilkdownEditor.vue'),
)
</script>

<template>
  <Suspense>
    <MilkdownProvider>
      <LazyMilkdownEditor
        v-model="documentation"
        v-model:editing="editing"
        :preferSingleLine="props.preferSingleLine"
      />
    </MilkdownProvider>
  </Suspense>
</template>
