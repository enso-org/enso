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
}>()
</script>

<template>
  <svg>
    <use :href="props.name.includes(':') ? props.name : `${icons}#${props.name}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: 16px;
  min-width: 16px;
  height: 16px;
  min-height: 16px;
}
</style>
