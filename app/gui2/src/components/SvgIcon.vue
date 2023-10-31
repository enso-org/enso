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
  <svg :style="{ '--width': `${width ?? 16}px`, '--height': `${height ?? 16}px` }">
    <use :href="`${icons}#${props.name}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: var(--width);
  min-width: var(--width);
  height: var(--height);
  min-height: var(--height);
}
</style>
