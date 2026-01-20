<script lang="ts">
import { svgUseHref, type AnyIcon } from '@/util/icons'
// This export is to have this widely used component's docs displayed in IDE.
/**
 * A component displaying a SVG icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 */
export default {}
</script>

<script setup lang="ts">
const { name } = defineProps<{ name: AnyIcon }>()
</script>

<template>
  <svg class="SvgIcon" viewBox="0 0 16 16" preserveAspectRatio="xMidYMid slice">
    <use :href="svgUseHref(name)"></use>
  </svg>
</template>

<style scoped>
.SvgIcon {
  overflow: visible; /* Prevent slight cutting off icons that are using all available space. */
  width: var(--icon-width, var(--icon-size, 16px));
  height: var(--icon-height, var(--icon-size, 16px));
  transform: var(--icon-transform);
  transform-origin: var(--icon-transform-origin, center center);
}
</style>
