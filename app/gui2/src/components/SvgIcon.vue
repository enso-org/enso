<script setup lang="ts">
/**
 * A component displaying a SVG icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 */
import icons from '@/assets/icons.svg'
import type { Icon } from '@/util/iconName'
import type { URLString } from '@/util/urlString'

const props = defineProps<{ name: Icon | URLString; width?: number; height?: number }>()
</script>

<template>
  <svg :style="{ '--width': `${width ?? 16}px`, '--height': `${height ?? 16}px` }">
    <use :href="props.name.includes(':') ? props.name : `${icons}#${props.name}`"></use>
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
