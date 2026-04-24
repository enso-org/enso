<script setup lang="ts">
import { providePopoverRoot } from '@/providers/popoverRoot'
import { useTemplateRef } from 'vue'

const root = useTemplateRef('root')
providePopoverRoot(root)
</script>

<template>
  <div ref="root" class="PopoverRootProvider">
    <slot />
  </div>
</template>

<style scoped>
.PopoverRootProvider {
  display: contents;
}
</style>
