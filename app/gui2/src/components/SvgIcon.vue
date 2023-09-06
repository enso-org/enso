<script setup lang="ts">
import icons from '@/assets/icons.svg'

const props = defineProps<{ variant: string }>()
</script>

<template>
  <svg>
    <use :href="`${icons}#${props.variant}`"></use>
  </svg>
</template>

<style>
svg {
  width: 16px;
  height: 16px;
}
</style>
