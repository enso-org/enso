<script setup lang="ts">
/**
 * A component displaying svg icon.
 *
 * It displays one group defined in `@/assets/icons.svg` file, specified by `variant` property.
 *
 */
import icons from '@/assets/icons.svg'

const props = defineProps<{ variant: string }>()
</script>

<template>
  <svg>
    <use :href="`${icons}#${props.variant}`"></use>
  </svg>
</template>

<style scoped>
svg {
  width: 16px;
  height: 16px;
}
</style>
