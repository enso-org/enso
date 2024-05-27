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
  title?: string
}>()
</script>

<template>
  <svg viewBox="0 0 16 16">
    <title v-if="title" v-text="title"></title>
    <use :href="props.name.includes(':') ? props.name : `${icons}#${props.name}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: var(--icon-size, 16px);
  min-width: var(--icon-size, 16px);
  height: var(--icon-size, 16px);
  min-height: var(--icon-size, 16px);
  transform: var(--icon-transform);
  transform-origin: var(--icon-transform-origin, center center);
}
</style>
