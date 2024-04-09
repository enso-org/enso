<script setup lang="ts">
/**
 * A component displaying a SVG icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 */
import icons from '@/assets/icons.svg'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconName'

const props = defineProps<{
  name: Icon | URLString
  width?: number
  height?: number
  scale?: number
}>()
</script>

<template>
  <svg
    :style="{
      '--width': `${props.width ?? 16}px`,
      '--height': `${props.height ?? 16}px`,
      '--scale': props.scale ?? 1,
    }"
  >
    <use :href="props.name.includes(':') ? props.name : `${icons}#${props.name}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: var(--width);
  min-width: var(--width);
  height: var(--height);
  min-height: var(--height);
  transform: scale(var(--scale));
  transform-origin: top left;
}
</style>
