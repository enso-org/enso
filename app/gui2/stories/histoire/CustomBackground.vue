<script lang="ts" setup>
import type { Story, Variant } from 'histoire'
import { computed } from 'vue'

const props = defineProps<{
  story: Story
  variant?: Variant
}>()

const color = computed(() => {
  return props.story.meta?.customBackground ?? props.variant?.meta?.customBackground ?? '#fff'
})

declare module 'histoire' {
  interface StoryMeta {
    customBackground?: string
  }

  interface VariantMeta {
    customBackground?: string
  }
}
</script>

<template>
  <div class="custom-background-wrapper" :style="{ backgroundColor: color }">
    <slot />
  </div>
</template>
