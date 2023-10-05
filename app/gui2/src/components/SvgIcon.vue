<script setup lang="ts">
/**
 * A component displaying svg icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 */
import icons from '@/assets/icons.svg'
import type { Icon } from '@/util/iconName'

const props = defineProps<{ name: Icon; width?: number; height?: number }>()
</script>

<template>
  <svg :style="{ '--width': `${width}px`, '--height': `${height}px` }">
    <use :href="`${icons}#${props.name}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: var(--width, 16px);
  min-width: var(--width, 16px);
  height: var(--height, 16px);
  min-height: var(--height, 16px);
}
</style>
